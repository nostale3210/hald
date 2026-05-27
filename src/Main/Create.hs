module Main.Create where

import Control.Exception (IOException, catch)
import Control.Monad (void, when)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.List (nubBy)
import Data.Maybe (fromMaybe)
import Main.CAS.Ingest qualified as CAS
import Main.Config qualified as Config
import Main.Deployment qualified as Dep
import Main.Util (TreeAction (..), WalkStrategy (..))
import Main.Util qualified as Util
import System.Directory (copyFile, copyFileWithMetadata, doesDirectoryExist, findExecutable, getSymbolicLinkTarget, removeFile)
import System.FilePath (makeRelative, takeDirectory, (</>))
import System.IO (hPutStrLn, stderr)
import System.Posix (fileMode, modificationTime, setFileMode, setFileTimesHiRes, setSymbolicLinkTimesHiRes)
import System.Process (readProcess)
import UnliftIO.Async (concurrently, pooledForConcurrently_)

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
          (\e -> Util.fatalWith (show (e :: IOException)) "")
      catch
        ( writeFile
            (Config.bootPath conf <> "/loader/entries/" <> show depId <> ".conf")
            (generateBootEntry depId templateContent)
        )
        (\e -> Util.fatal $ "Couldn't create boot entry!\n" <> show (e :: IOException))
    else Util.fatal $ "No boot entry template found, make sure it exists at " <> Config.configPath conf <> "/boot.conf"

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
    (\e -> Util.fatal $ "Couldn't sync system config!\n" <> show (e :: IOException))
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
      (\(_ :: IOException) -> Util.fatalWith "Failed to merge files!" "")
  contentB <-
    catch
      (readFile inputB)
      (\(_ :: IOException) -> Util.fatalWith "Failed to merge files!" "")
  let output =
        unlines
          . nubBy (\a b -> Util.takeUntil a ':' == Util.takeUntil b ':')
          . lines
          $ contentA <> contentB
  catch
    (writeFile outputFile output)
    (\(_ :: IOException) -> Util.fatal "Failed to merge files!")

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
  Util.walk
    (ParallelN 2)
    ( TreeAction
        { dirAction = \_ _ -> return (),
          symAction = \p s ->
            when (modificationTime s /= 0) $
              atomicModifyIORef' ref (\acc -> (p : acc, ())),
          fileAction = \p s ->
            when (modificationTime s /= 0) $
              atomicModifyIORef' ref (\acc -> (p : acc, ()))
        }
    )
    root
  readIORef ref

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
syncSingle path target = Util.walk Sequential action path
  where
    action =
      TreeAction
        { dirAction = \p s -> do
            let d = target <> p
            Util.ensureDirExists d
            setFileMode d (fileMode s),
          symAction = \p _ -> do
            let d = target <> p
            symTarget <- getSymbolicLinkTarget p
            Util.ensureDirExists (takeDirectory d)
            Util.createSymlink symTarget d,
          fileAction = \p _ -> do
            let d = target <> p
            Util.ensureDirExists (takeDirectory d)
            copyFileWithMetadata p d
        }

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
  catch (void $ readProcess "rsync" rsyncCmd "") (\e -> Util.fatal $ "Syncing deployment /usr failed: " <> show (e :: IOException))
  catch
    (writeFile (depPath <> "/usr/.ald_dep") (show (Dep.identifier dep)))
    (\e -> Util.fatal $ "Writing deployment marker failed: " <> show (e :: IOException))

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
    (\e -> Util.fatal $ "Writing deployment marker failed: " <> show (e :: IOException))

syncDeploymentEtc :: FilePath -> Config.Config -> Dep.Deployment -> IO ()
syncDeploymentEtc containerMount conf dep = do
  let depPath = Config.haldPath conf </> show (Dep.identifier dep)
      depEtc = depPath <> "/etc"
      containerEtc = containerMount <> "/etc"
  Util.ensureDirExists depEtc
  catch
    (copyTree containerEtc depEtc)
    (\e -> Util.fatal $ "Syncing deployment /etc failed: " <> show (e :: IOException))

copyTree :: FilePath -> FilePath -> IO ()
copyTree src dst =
  Util.walk (ParallelN 2) action src
  where
    action =
      TreeAction
        { dirAction = \p s -> do
            let d = dst </> makeRelative src p
            Util.ensureDirExists d
            setFileMode d (fileMode s),
          symAction = \p _ -> do
            let d = dst </> makeRelative src p
            symTarget <- getSymbolicLinkTarget p
            Util.ensureDirExists (takeDirectory d)
            Util.createSymlink symTarget d,
          fileAction = \p _ -> do
            let d = dst </> makeRelative src p
            Util.ensureDirExists (takeDirectory d)
            copyFileWithMetadata p d
        }

normalizeDepEtcTimestamps :: Config.Config -> Dep.Deployment -> IO ()
normalizeDepEtcTimestamps conf dep =
  Util.walk Sequential action (Config.haldPath conf </> show (Dep.identifier dep) <> "/etc")
  where
    action =
      TreeAction
        { dirAction = \p _ -> setFileTimesHiRes p 0 0,
          symAction = \p _ -> setSymbolicLinkTimesHiRes p 0 0,
          fileAction = \p _ -> setFileTimesHiRes p 0 0
        }

copyContainerFiles :: Config.Config -> Dep.Deployment -> IO ()
copyContainerFiles conf dep = do
  let hp = Config.haldPath conf
      depId = Dep.identifier dep
  catch
    (void $ Util.quietReadProcess "podman" ["cp", "-a", "ald-root:/files", hp <> "/." <> show depId] "")
    ( \(_ :: IOException) ->
        catch
          (writeFile (hp <> "/." <> show depId) "")
          (\(_ :: IOException) -> Util.fatal "")
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
    [] -> Util.fatalWith ("No " <> target <> " found in deployment " <> show (Dep.identifier deployment)) ""
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
    Nothing -> Util.fatal $ "No boot directory supplied for deployment " <> show (Dep.identifier deployment)

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
      (\e -> Util.fatalWith (show (e :: IOException)) "")
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
            Util.quietReadProcess
              "ukify"
              [ "build",
                "--linux=" <> kernel,
                "--initrd=" <> initrd,
                "--cmdline=" <> cmdline,
                "--output=" <> x
              ]
              ""
        )
        (\e -> Util.fatal $ "ukify build failed: " <> show (e :: IOException))
    Nothing -> Util.fatal $ "No UKI path supplied for deployment " <> show (Dep.identifier deployment)

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
    (\(_ :: IOException) -> Util.fatal "Failed fetching package db!")

setDefaultBootEntry :: Int -> IO ()
setDefaultBootEntry dep = do
  r <- findExecutable "bootctl"
  case r of
    Nothing -> return ()
    Just _ ->
      catch
        (void $ readProcess "bootctl" ["set-default", "*" <> show dep <> "*"] "")
        (\e -> let err = show (e :: IOException) in hPutStrLn stderr $ "Failed setting default boot entry: " <> err)
