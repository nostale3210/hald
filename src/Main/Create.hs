module Main.Create where

import Control.Exception (IOException, catch)
import Control.Monad (when)
import Data.List (nubBy)
import Data.Maybe (fromMaybe)
import Main.CAS.Ingest qualified as CAS
import Main.Config qualified as Config
import Main.Deployment qualified as Dep
import Main.Util qualified as Util
import System.Directory (copyFile, doesDirectoryExist, removeFile)
import System.FilePath (takeDirectory, (</>))
import System.IO (hPutStrLn, stderr)
import System.Posix.Signals (raiseSignal, sigTERM)
import System.Process (callCommand)
import UnliftIO.Async (concurrently, pooledForConcurrentlyN_)
import UnliftIO.Concurrent (getNumCapabilities, threadDelay)

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
    else syncState conf depPath
  catch
    ( callCommand
        ( "podman cp ald-root:/etc/passwd "
            <> depPath
            <> "/.tmp.passwd && podman cp ald-root:/etc/shadow "
            <> depPath
            <> "/.tmp.shadow && podman cp ald-root:/etc/group "
            <> depPath
            <> "/.tmp.group"
        )
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
syncMinimumState depPath =
  syncSingleFile
    ( words
        ( "/etc/fstab /etc/crypttab /etc/locale.conf /etc/localtime "
            <> "/etc/adjtime /etc/sudoers.d /etc/group /etc/gshadow /etc/subgid /etc/subuid "
            <> "/etc/NetworkManager/system-connections /etc/vconsole.conf /etc/pki "
            <> "/etc/firewalld /etc/environment /etc/hostname "
            <> "/etc/X11/xorg.conf.d/00-keyboard.conf /etc/sudoers /etc/hald /etc/machine-id "
            <> "/etc/X11/xinit/xinput.d/ibus.conf"
        )
    )
    (depPath <> "/")

syncState :: Config.Config -> FilePath -> IO ()
syncState conf depPath =
  let syncCmd = "cp --parents -a \"$@\" " <> depPath <> " >/dev/null 2>&1 || :"
      xargsSync = "xargs -n1 -P\"$(($(nproc --all)/2))\" bash -c '" <> syncCmd <> "' _"
   in catch
        ( callCommand
            ( "touch -d 1970-01-01T01:00:00 "
                <> Config.configPath conf
                <> "/.stamp &&"
                <> "find /etc ! -type d -newer "
                <> Config.configPath conf
                <> "/.stamp"
                <> " | "
                <> xargsSync
            )
        )
        ( \e ->
            let err = show (e :: IOException)
             in hPutStrLn stderr err
                  >> raiseSignal sigTERM
                  >> threadDelay maxBound
        )
        >> syncMinimumState depPath

syncSingleFile :: [FilePath] -> FilePath -> IO ()
syncSingleFile files destination
  | null files = return ()
  | otherwise = do
      threads <- getNumCapabilities
      let workThreads = max 1 $ div threads 2
      destExists <- doesDirectoryExist destination
      when destExists $ pooledForConcurrentlyN_ workThreads files $ \p ->
        catch
          ( callCommand
              ( "cp --parents -a  "
                  <> p
                  <> " "
                  <> destination
              )
          )
          ( \e ->
              let _ = show (e :: IOException)
               in Util.printInfo
                    ( "Some required files couldn't be synchronized.\n"
                        <> "Manual intervention might be necessary"
                    )
                    False
          )

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
  let rsyncBase = "rsync -aHlx " <> containerMount <> "/usr/ " <> depUsr <> "/"
      rsyncCmd = case linkSource of
        Just src -> rsyncBase <> " --link-dest=../../" <> show src <> "/usr"
        Nothing -> rsyncBase
  catch (callCommand rsyncCmd) (\e -> hPutStrLn stderr ("Syncing deployment /usr failed: " <> show (e :: IOException)) >> raiseSignal sigTERM >> threadDelay maxBound)
  catch
    (writeFile (depPath <> "/usr/.ald_dep") (show (Dep.identifier dep)))
    (\e -> hPutStrLn stderr ("Writing deployment marker failed: " <> show (e :: IOException)) >> raiseSignal sigTERM >> threadDelay maxBound)

syncDeploymentUsrCas :: FilePath -> Config.Config -> Dep.Deployment -> IO ()
syncDeploymentUsrCas containerMount conf dep = do
  let depPath = Config.haldPath conf </> show (Dep.identifier dep)
      casDir = Config.haldPath conf <> "/objects"
      depUsr = depPath <> "/usr"
  Util.ensureDirExists casDir
  Util.ensureDirExists depUsr
  assetMap <- CAS.ingestPath (containerMount <> "/usr") casDir
  CAS.deployTree casDir depUsr assetMap
  CAS.saveAssetMap (depPath <> "/.ald_assetmap") assetMap
  catch
    ( writeFile
        (depPath <> "/usr/.ald_dep")
        (show (Dep.identifier dep))
    )
    (\e -> hPutStrLn stderr ("Writing deployment marker failed: " <> show (e :: IOException)) >> raiseSignal sigTERM >> threadDelay maxBound)

syncDeploymentEtc :: FilePath -> Config.Config -> Dep.Deployment -> IO ()
syncDeploymentEtc containerMount conf dep = do
  let depPath = Config.haldPath conf </> show (Dep.identifier dep)
  catch
    ( callCommand
        ( "rsync -aHlx "
            <> containerMount
            <> "/etc/ "
            <> depPath
            <> "/etc/"
        )
    )
    (\e -> hPutStrLn stderr ("Syncing deployment /etc failed: " <> show (e :: IOException)) >> raiseSignal sigTERM >> threadDelay maxBound)

normalizeDepEtcTimestamps :: Config.Config -> Dep.Deployment -> IO ()
normalizeDepEtcTimestamps conf dep = do
  let depPath = Config.haldPath conf </> show (Dep.identifier dep)
  catch
    ( callCommand
        ( "find "
            <> depPath
            <> "/etc -mindepth 1 "
            <> "-execdir sh -c \"touch -d 1970-01-01T01:00:00 '{}' >/dev/null 2>&1 || :\" \\;"
        )
    )
    (\e -> hPutStrLn stderr ("Normalizing /etc timestamps failed: " <> show (e :: IOException)) >> raiseSignal sigTERM >> threadDelay maxBound)

copyContainerFiles :: Config.Config -> Dep.Deployment -> IO ()
copyContainerFiles conf dep = do
  let hp = Config.haldPath conf
      depId = Dep.identifier dep
  catch
    ( callCommand
        ( "podman cp -a ald-root:/files "
            <> hp
            <> "/."
            <> show depId
            <> " >/dev/null 2>&1"
        )
    )
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
        ( callCommand
            ( "ukify build --linux="
                <> kernel
                <> " --initrd="
                <> initrd
                <> " --cmdline='"
                <> cmdline
                <> "' --output="
                <> x
                <> " >/dev/null 2>&1"
            )
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
        >> callCommand
          ( "rsync -a "
              <> containerPath
              <> fromMaybe "" (Config.packageDB conf)
              <> " "
              <> fromMaybe "" (Dep.rootDir dep)
              <> "/"
              <> takeDirectory (fromMaybe "" (Config.packageDB conf))
              <> "/"
          )
    )
    ( \e ->
        let _ = show (e :: IOException)
         in hPutStrLn stderr "Failed fetching package db!"
              >> raiseSignal sigTERM
              >> threadDelay maxBound
    )

setDefaultBootEntry :: Int -> IO ()
setDefaultBootEntry dep =
  catch
    ( callCommand
        ("hash bootctl 2>/dev/null && bootctl set-default \"*" <> show dep <> "*\"")
    )
    ( \e ->
        let _ = show (e :: IOException)
         in putStr ""
    )
