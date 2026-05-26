{-# LANGUAGE MultiWayIf #-}

module Main.Create where

import Control.Exception (IOException, catch)
import Control.Monad (forM_, void, when)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.List (nubBy)
import Data.Maybe (fromMaybe)
import Main.CAS.Ingest qualified as CAS
import Main.Config qualified as Config
import Main.Deployment qualified as Dep
import Main.Util qualified as Util
import System.Directory (copyFile, copyFileWithMetadata, doesDirectoryExist, findExecutable, getSymbolicLinkTarget, listDirectory, removeFile)
import System.FilePath (takeDirectory, (</>))
import System.IO (hPutStrLn, stderr)
import System.IO.Error (isAlreadyExistsError)
import System.Posix (createSymbolicLink, fileMode, isDirectory, isSymbolicLink, modificationTime, setFileMode, setFileTimes)
import System.Posix.Signals (raiseSignal, sigTERM)
import System.Process (readProcess)
import UnliftIO.Async (concurrently, pooledForConcurrentlyN_, pooledForConcurrently_)
import UnliftIO.Concurrent (threadDelay)

createSkeleton :: Int -> Config.Config -> Bool -> Dep.Backend -> IO ()
createSkeleton depId conf uki backend =
  Util.ensureDirExists (Config.haldPath conf </> show depId)
    >> writeFile (Config.haldPath conf </> show depId <> "/.backend") (show backend)
    >> Util.createSymlink "usr/lib" (Config.haldPath conf </> show depId <> "/lib")
    >> if uki
      then Util.ensureDirExists (Config.ukiPath conf)
      else Util.ensureDirExists (Config.bootPath conf </> show depId)

createBootEntry :: Int -> Config.Config -> IO ()
createBootEntry depId conf = do
  Util.ensureDirExists $ Config.bootPath conf <> "/loader/entries"
  bootTemplateExists <- Util.pathExists (Config.configPath conf <> "/boot.conf")
  if bootTemplateExists
    then do
      templateContent <-
        catch
          ( readFile $
              Config.configPath conf <> "/boot.conf"
          )
          ( \e ->
              let err = show (e :: IOException)
               in hPutStrLn stderr err
                    >> raiseSignal sigTERM
                    >> threadDelay maxBound
                    >> return ""
          )
      catch
        ( writeFile
            (Config.bootPath conf <> "/loader/entries/" <> show depId <> ".conf")
            (generateBootEntry depId templateContent)
        )
        ( \e ->
            let err = show (e :: IOException)
             in hPutStrLn stderr ("Couldn't create boot entry!\n" <> err)
                  >> raiseSignal sigTERM
                  >> threadDelay maxBound
        )
    else
      hPutStrLn stderr ("No boot entry template found, make sure it exists at " <> Config.configPath conf <> "/boot.conf")
        >> raiseSignal sigTERM
        >> threadDelay maxBound

generateBootEntry :: Int -> String -> String
generateBootEntry depId = Util.replaceString "INSERT_DEPLOYMENT" (show depId)

syncSystemConfig :: Bool -> Config.Config -> Dep.Deployment -> IO ()
syncSystemConfig dropState conf dep = do
  let depPath = Config.haldPath conf </> show (Dep.identifier dep)
  if dropState
    then syncMinimumState depPath
    else syncState depPath
  catch
    ( do
        void $ readProcess "podman" ["cp", "ald-root:/etc/passwd", depPath <> "/.tmp.passwd"] ""
        void $ readProcess "podman" ["cp", "ald-root:/etc/shadow", depPath <> "/.tmp.shadow"] ""
        void $ readProcess "podman" ["cp", "ald-root:/etc/group", depPath <> "/.tmp.group"] ""
    )
    ( \e ->
        let err = show (e :: IOException)
         in hPutStrLn stderr ("Couldn't sync system config!\n" <> err)
              >> raiseSignal sigTERM
              >> threadDelay maxBound
    )
  mergeFiles "/etc/passwd" (depPath <> "/.tmp.passwd") (depPath <> "/etc/passwd")
    >> removeTmpFile (depPath <> "/.tmp.passwd")
  mergeFiles "/etc/shadow" (depPath <> "/.tmp.shadow") (depPath <> "/etc/shadow")
    >> removeTmpFile (depPath <> "/.tmp.shadow")
  mergeFiles "/etc/group" (depPath <> "/.tmp.group") (depPath <> "/etc/group")
    >> removeTmpFile (depPath <> "/.tmp.group")

removeTmpFile :: FilePath -> IO ()
removeTmpFile file =
  Util.pathExists file >>= \fileExists ->
    when fileExists $
      catch
        ( removeFile file
        )
        ( \e ->
            let err = show (e :: IOException)
             in Util.printInfo ("Couldn't remove temporary files; " <> err) False
        )

mergeFiles :: FilePath -> FilePath -> FilePath -> IO ()
mergeFiles inputA inputB outputFile = do
  contentA <-
    catch
      (readFile inputA)
      ( \e ->
          let _ = show (e :: IOException)
           in hPutStrLn stderr "Failed to merge files!"
                >> raiseSignal sigTERM
                >> threadDelay maxBound
                >> return ""
      )
  contentB <-
    catch
      (readFile inputB)
      ( \e ->
          let _ = show (e :: IOException)
           in hPutStrLn stderr "Failed to merge files!"
                >> raiseSignal sigTERM
                >> threadDelay maxBound
                >> return ""
      )
  let output =
        unlines
          . nubBy (\a b -> Util.takeUntil a ':' == Util.takeUntil b ':')
          . lines
          $ contentA <> contentB
  catch
    (writeFile outputFile output)
    ( \e ->
        let _ = show (e :: IOException)
         in hPutStrLn stderr "Failed to merge files!"
              >> raiseSignal sigTERM
              >> threadDelay maxBound
    )

syncMinimumState :: FilePath -> IO ()
syncMinimumState =
  syncSingleFile
    ( words
        ( "/etc/fstab /etc/crypttab /etc/locale.conf /etc/localtime "
            <> "/etc/adjtime /etc/sudoers.d /etc/group /etc/gshadow /etc/subgid /etc/subuid "
            <> "/etc/NetworkManager/system-connections /etc/vconsole.conf /etc/pki "
            <> "/etc/firewalld /etc/environment /etc/hostname "
            <> "/etc/X11/xorg.conf.d/00-keyboard.conf /etc/sudoers /etc/hald /etc/machine-id "
        )
    )

syncState :: FilePath -> IO ()
syncState depPath = do
  let depEtc = depPath <> "/etc"
  Util.ensureDirExists depEtc
  files <- collectFiles "/etc"
  destExists <- doesDirectoryExist depPath
  when destExists $
    pooledForConcurrently_ files $ \p ->
      syncSingle p depPath
  syncMinimumState depPath

collectFiles :: FilePath -> IO [FilePath]
collectFiles root = do
  ref <- newIORef []
  walk root ref
  readIORef ref
  where
    walk dir ref = do
      entries <- catch (listDirectory dir) (\e -> let _ = show (e :: IOException) in return [])
      pooledForConcurrentlyN_ 2 entries $ \entry -> do
        let path = dir </> entry
        mStat <- Util.tryStat path
        case mStat of
          Nothing -> return ()
          Just s
            | modificationTime s == 0 -> return ()
            | isDirectory s -> walk path ref
            | otherwise ->
                atomicModifyIORef' ref (\acc -> (path : acc, ()))

syncSingleFile :: [FilePath] -> FilePath -> IO ()
syncSingleFile files destination
  | null files = return ()
  | otherwise = do
      destExists <- doesDirectoryExist destination
      when destExists $ pooledForConcurrently_ files $ \p ->
        catch
          (syncSingle p destination)
          ( \e ->
              let _ = show (e :: IOException)
               in Util.printInfo
                    ( "Some required files couldn't be synchronized: "
                        <> p
                        <> "\n"
                        <> "Manual intervention might be necessary"
                    )
                    False
          )

syncSingle :: FilePath -> FilePath -> IO ()
syncSingle path target = do
  stat <- Util.tryStat path
  let fullTarget = target <> path
  case stat of
    Just x ->
      if
        | isDirectory x -> do
            Util.ensureDirExists fullTarget
            setFileMode fullTarget (fileMode x)
            dirEntries <- listDirectory path
            forM_ dirEntries $ \entry ->
              syncSingle (path </> entry) target
        | isSymbolicLink x -> do
            symTarget <- getSymbolicLinkTarget path
            Util.ensureDirExists (takeDirectory fullTarget)
            catch
              (createSymbolicLink symTarget fullTarget)
              ( \e ->
                  if isAlreadyExistsError e then return () else ioError e
              )
        | otherwise -> do
            Util.ensureDirExists (takeDirectory fullTarget)
            copyFileWithMetadata path fullTarget
    Nothing -> void $ ioError (userError "Couldn't stat path!")

syncDeploymentUsr :: FilePath -> Config.Config -> Dep.Deployment -> Maybe Int -> IO ()
syncDeploymentUsr containerMount conf dep linkSource =
  case Dep.backend dep of
    Dep.Hardlink -> syncDeploymentUsrHardlink containerMount conf dep linkSource
    Dep.Cas -> syncDeploymentUsrCas containerMount conf dep

syncDeploymentUsrHardlink :: FilePath -> Config.Config -> Dep.Deployment -> Maybe Int -> IO ()
syncDeploymentUsrHardlink containerMount conf dep linkSource = do
  let depPath = Config.haldPath conf </> show (Dep.identifier dep)
      depUsr = depPath <> "/usr"
  Util.ensureDirExists depUsr
  let rsyncArgs = ["-aHlx", containerMount <> "/usr/", depUsr <> "/"]
      rsyncCmd = case linkSource of
        Just src -> rsyncArgs <> ["--link-dest=../../" <> show src <> "/usr"]
        Nothing -> rsyncArgs
  catch (void $ readProcess "rsync" rsyncCmd "") (\e -> hPutStrLn stderr ("Syncing deployment /usr failed: " <> show (e :: IOException)) >> raiseSignal sigTERM >> threadDelay maxBound)
  catch
    (writeFile (depPath <> "/usr/.ald_dep") (show (Dep.identifier dep)))
    (\e -> hPutStrLn stderr ("Writing deployment marker failed: " <> show (e :: IOException)) >> raiseSignal sigTERM >> threadDelay maxBound)

syncDeploymentUsrCas :: FilePath -> Config.Config -> Dep.Deployment -> IO ()
syncDeploymentUsrCas containerMount conf dep = do
  let depPath = Config.haldPath conf </> show (Dep.identifier dep)
      casDir = Config.haldPath conf <> "/objects"
      depUsr = depPath <> "/usr"
      assetMapPath = depPath <> "/.ald_assetmap"
  Util.ensureDirExists casDir
  Util.ensureDirExists depUsr
  CAS.ingestTree (containerMount <> "/usr") casDir assetMapPath
  CAS.deployTreeFromFile casDir depUsr assetMapPath
  catch
    ( writeFile
        (depPath <> "/usr/.ald_dep")
        (show (Dep.identifier dep))
    )
    (\e -> hPutStrLn stderr ("Writing deployment marker failed: " <> show (e :: IOException)) >> raiseSignal sigTERM >> threadDelay maxBound)

syncDeploymentEtc :: FilePath -> Config.Config -> Dep.Deployment -> IO ()
syncDeploymentEtc containerMount conf dep = do
  let depPath = Config.haldPath conf </> show (Dep.identifier dep)
      depEtc = depPath <> "/etc"
  Util.ensureDirExists depEtc
  catch
    ( void $
        readProcess
          "cp"
          ["-a", containerMount <> "/etc/.", depEtc <> "/."]
          ""
    )
    (\e -> hPutStrLn stderr ("Syncing deployment /etc failed: " <> show (e :: IOException)) >> raiseSignal sigTERM >> threadDelay maxBound)

normalizeDepEtcTimestamps :: Config.Config -> Dep.Deployment -> IO ()
normalizeDepEtcTimestamps conf dep = do
  let depEtc = Config.haldPath conf </> show (Dep.identifier dep) <> "/etc"
  etcExists <- doesDirectoryExist depEtc
  when etcExists $
    walk depEtc
  where
    walk dir = do
      entries <- catch (listDirectory dir) (\e -> let _ = show (e :: IOException) in return [])
      forM_ entries $ \entry -> do
        let path = dir </> entry
        stat <- Util.tryStat path
        case stat of
          Nothing -> return ()
          Just s
            | isDirectory s -> setFileTimes path 0 0 >> walk path
            | isSymbolicLink s -> return ()
            | otherwise -> setFileTimes path 0 0

copyContainerFiles :: Config.Config -> Dep.Deployment -> IO ()
copyContainerFiles conf dep = do
  let hp = Config.haldPath conf
      depId = Dep.identifier dep
  catch
    (void $ readProcess "podman" ["cp", "-a", "ald-root:/files", hp <> "/." <> show depId] "")
    ( \e ->
        let _ = show (e :: IOException)
         in catch
              (writeFile (hp <> "/." <> show depId) "")
              (\e2 -> let _ = (e2 :: IOException) in raiseSignal sigTERM >> threadDelay maxBound)
    )

modulePathSearch :: Config.Config -> Dep.Deployment -> FilePath -> IO FilePath
modulePathSearch conf deployment target = do
  paths <-
    Util.recursiveFileSearch
      ( Config.haldPath conf
          <> "/"
          <> show (Dep.identifier deployment)
          <> "/usr/lib/modules"
      )
      target
  case paths of
    [] -> do
      hPutStrLn stderr ("No " <> target <> " found in deployment " <> show (Dep.identifier deployment))
      raiseSignal sigTERM
      threadDelay maxBound
      return ""
    x : _ -> return x

placeBootFiles :: Config.Config -> Dep.Deployment -> IO ()
placeBootFiles conf deployment = do
  (kernel, initrd) <-
    concurrently
      (modulePathSearch conf deployment "vmlinuz")
      (modulePathSearch conf deployment "initramfs.img")
  let bootComps = Dep.bootComponents deployment
  case Dep.bootDir bootComps of
    Just x -> do
      copyFile kernel (x <> "/vmlinuz")
      copyFile initrd (x <> "/initramfs.img")
    Nothing -> do
      hPutStrLn stderr ("No boot directory supplied for deployment " <> show (Dep.identifier deployment))
      raiseSignal sigTERM
      threadDelay maxBound

installUki :: Config.Config -> Dep.Deployment -> IO ()
installUki conf deployment = do
  (kernel, initrd) <-
    concurrently
      (modulePathSearch conf deployment "vmlinuz")
      (modulePathSearch conf deployment "initramfs.img")
  templCmdline <-
    catch
      ( readFile $
          Config.configPath conf <> "/cmdline"
      )
      ( \e ->
          let err = show (e :: IOException)
           in hPutStrLn stderr err
                >> raiseSignal sigTERM
                >> threadDelay maxBound
                >> return ""
      )
  let cmdline =
        Util.removeString "\n" $
          Util.replaceString
            "INSERT_DEPLOYMENT"
            (show $ Dep.identifier deployment)
            templCmdline
  Util.printInfo ("UKI cmdline: " <> cmdline) (Config.interactive conf)
  let bootComps = Dep.bootComponents deployment
  case Dep.ukiPath bootComps of
    Just x ->
      catch
        ( void $
            readProcess
              "ukify"
              [ "build",
                "--linux=" <> kernel,
                "--initrd=" <> initrd,
                "--cmdline=" <> cmdline,
                "--output=" <> x
              ]
              ""
        )
        ( \e -> hPutStrLn stderr ("ukify build failed: " <> show (e :: IOException)) >> raiseSignal sigTERM >> threadDelay maxBound
        )
    Nothing -> do
      hPutStrLn stderr ("No UKI path supplied for deployment " <> show (Dep.identifier deployment))
      raiseSignal sigTERM
      threadDelay maxBound

getPackageDB :: FilePath -> Config.Config -> Dep.Deployment -> IO ()
getPackageDB containerPath conf dep =
  catch
    ( Util.ensureDirExists
        ( Config.haldPath conf
            <> "/"
            <> show (Dep.identifier dep)
            <> "/"
            <> takeDirectory (fromMaybe "" (Config.packageDB conf))
        )
        >> readProcess
          "rsync"
          [ "-a",
            containerPath <> fromMaybe "" (Config.packageDB conf),
            fromMaybe "" (Dep.rootDir dep) <> "/" <> takeDirectory (fromMaybe "" (Config.packageDB conf)) <> "/"
          ]
          ""
        >> return ()
    )
    ( \e ->
        let _ = show (e :: IOException)
         in hPutStrLn stderr "Failed fetching package db!"
              >> raiseSignal sigTERM
              >> threadDelay maxBound
    )

setDefaultBootEntry :: Int -> IO ()
setDefaultBootEntry dep = do
  r <- findExecutable "bootctl"
  case r of
    Nothing -> return ()
    Just _ ->
      catch
        (void $ readProcess "bootctl" ["set-default", "*" <> show dep <> "*"] "")
        (\e -> let err = show (e :: IOException) in hPutStrLn stderr $ "Failed setting default boot entry: " <> err)
