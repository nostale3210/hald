module Hald.Create where

import Control.Concurrent.STM (atomically, modifyTVar', newTVarIO, readTVarIO)
import Control.Exception (IOException, catch)
import Control.Monad (void, when)
import Data.List (nubBy)
import Data.Maybe (fromMaybe, listToMaybe)
import Hald.Cas.Ingest qualified as CAS
import Hald.Config qualified as Config
import Hald.Deployment qualified as Dep
import Hald.Legacy qualified as Legacy
import Hald.Lock qualified as Lock
import Hald.Util (TreeAction (..), WalkStrategy (..))
import Hald.Util qualified as Util
import System.Directory (copyFile, copyFileWithMetadata, doesDirectoryExist, findExecutable, getSymbolicLinkTarget, removeFile)
import System.FilePath (makeRelative, takeDirectory, (</>))
import System.IO (hPutStrLn, stderr)
import System.Posix (fileMode, modificationTime, setFileMode, setFileTimesHiRes, setSymbolicLinkTimesHiRes)
import System.Process (readProcess)
import UnliftIO.Async (concurrently, pooledForConcurrently_)

createSkeleton :: Int -> Config.Config -> Bool -> Dep.Backend -> IO ()
createSkeleton depId conf uki backend =
  Util.ensureDirExists (Legacy.treeRootDir conf depId)
    >> writeFile (Legacy.treeRootDir conf depId <> "/backend") (show backend)
    >> Util.createSymlink "usr/lib" (Legacy.treeRootDir conf depId <> "/lib")
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
        Util.ioOrDie "Reading boot entry template" $
          readFile $
            Config.configPath conf <> "/boot.conf"
      Util.ioOrDie "Creating boot entry" $
        writeFile
          (Config.bootPath conf <> "/loader/entries/" <> show depId <> ".conf")
          (generateBootEntry depId templateContent)
    else Util.fatal $ "No boot entry template found, make sure it exists at " <> Config.configPath conf <> "/boot.conf"

generateBootEntry :: Int -> String -> String
generateBootEntry depId = Util.replaceString "INSERT_DEPLOYMENT" (show depId)

syncSystemConfig :: Bool -> Config.Config -> Dep.Deployment -> IO ()
syncSystemConfig dropState conf dep = do
  let depPath = Legacy.treeRootDir conf (Dep.identifier dep)
  if dropState
    then syncMinimumState depPath
    else syncState depPath
  Util.ioOrDie "Syncing system config" $ do
    void $ readProcess "podman" ["cp", "hald-root:/etc/passwd", depPath <> "/.tmp.passwd"] ""
    void $ readProcess "podman" ["cp", "hald-root:/etc/shadow", depPath <> "/.tmp.shadow"] ""
    void $ readProcess "podman" ["cp", "hald-root:/etc/group", depPath <> "/.tmp.group"] ""
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

mergeFileContents :: String -> String -> String
mergeFileContents a b =
  unlines . nubBy (\x y -> Util.takeUntil x ':' == Util.takeUntil y ':') . lines $
    a <> b

mergeFiles :: FilePath -> FilePath -> FilePath -> IO ()
mergeFiles inputA inputB outputFile = do
  contentA <- Util.ioOrDie "Reading merge file A" $ readFile inputA
  contentB <- Util.ioOrDie "Reading merge file B" $ readFile inputB
  Util.ioOrDie "Writing merged file" $
    writeFile outputFile $
      mergeFileContents contentA contentB

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
  ref <- newTVarIO []
  Util.walk
    (ParallelN 2)
    ( TreeAction
        { dirAction = \_ _ -> return (),
          symAction = \p s ->
            when (modificationTime s /= 0) $
              atomically $
                modifyTVar' ref (p :),
          fileAction = \p s ->
            when (modificationTime s /= 0) $
              atomically $
                modifyTVar' ref (p :)
        }
    )
    root
  readTVarIO ref

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

syncDeploymentUsr :: FilePath -> Config.Config -> Dep.Deployment -> Maybe Int -> [FilePath] -> IO ()
syncDeploymentUsr containerMount conf dep linkSource layerDiffs =
  case Dep.backend dep of
    Dep.Hardlink -> syncDeploymentUsrHardlink containerMount conf dep linkSource
    Dep.Cas -> syncDeploymentUsrCas containerMount conf dep layerDiffs

syncDeploymentUsrHardlink :: FilePath -> Config.Config -> Dep.Deployment -> Maybe Int -> IO ()
syncDeploymentUsrHardlink containerMount conf dep linkSource = do
  let depPath = Legacy.treeRootDir conf (Dep.identifier dep)
      depUsr = depPath <> "/usr"
  Util.ensureDirExists depUsr
  let rsyncArgs = ["-aHlx", containerMount <> "/usr/", depUsr <> "/"]
      rsyncCmd = case linkSource of
        Just src -> rsyncArgs <> ["--link-dest=../../" <> show src <> "/usr"]
        Nothing -> rsyncArgs
  Util.ioOrDie "Syncing deployment /usr" $ void $ readProcess "rsync" rsyncCmd ""
  Util.ioOrDie "Writing deployment marker" $
    writeFile (depPath <> "/usr/.hald_dep") (show (Dep.identifier dep))

syncDeploymentUsrCas :: FilePath -> Config.Config -> Dep.Deployment -> [FilePath] -> IO ()
syncDeploymentUsrCas containerMount conf dep layerDiffs = do
  let depPath = Legacy.treeRootDir conf (Dep.identifier dep)
      casDir = Config.haldPath conf <> "/objects"
      depUsr = depPath <> "/usr"
      assetMapPath = depPath <> "/assetmap"
      emptyFile = depPath <> "/empty"
  Util.ensureDirExists casDir
  Util.ensureDirExists depUsr
  Util.ioOrDie "Creating canonical empty file" $ writeFile emptyFile ""
  CAS.ingestTree containerMount "usr" casDir assetMapPath layerDiffs
  CAS.deployTreeFromFile casDir depUsr emptyFile assetMapPath
  Lock.setImmutable emptyFile
  Util.ioOrDie "Writing deployment marker" $ do
    let depMarker = depPath <> "/usr/.hald_dep"
    writeFile depMarker (show (Dep.identifier dep))
    Lock.setImmutable depMarker

syncDeploymentEtc :: FilePath -> Config.Config -> Dep.Deployment -> IO ()
syncDeploymentEtc containerMount conf dep = do
  let depPath = Legacy.treeRootDir conf (Dep.identifier dep)
      depEtc = depPath <> "/etc"
      containerEtc = containerMount <> "/etc"
  Util.ensureDirExists depEtc
  Util.ioOrDie "Syncing deployment /etc" $ copyTree containerEtc depEtc

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
  Util.walk Sequential action (Legacy.treeRootDir conf (Dep.identifier dep) <> "/etc")
  where
    action =
      TreeAction
        { dirAction = \p _ -> setFileTimesHiRes p 0 0,
          symAction = \p _ -> setSymbolicLinkTimesHiRes p 0 0,
          fileAction = \p _ -> setFileTimesHiRes p 0 0
        }

writeLockfile :: Config.Config -> Dep.Deployment -> IO ()
writeLockfile conf dep = do
  let depId = Dep.identifier dep
  Util.ioOrDie "Writing lockfile" $ writeFile (Legacy.treeLockfile conf depId) ""

modulePathSearch :: Config.Config -> Dep.Deployment -> FilePath -> IO FilePath
modulePathSearch conf deployment target =
  Util.recursiveFileSearch
    (Legacy.treeRootDir conf (Dep.identifier deployment) <> "/usr/lib/modules")
    target
    >>= maybe
      ( Util.fatalWith
          ("No " <> target <> " found in deployment " <> show (Dep.identifier deployment))
          ""
      )
      return
      . listToMaybe

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
    Util.ioOrDie "Reading UKI cmdline" $
      readFile $
        Config.configPath conf <> "/cmdline"
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
      Util.ioOrDie "Building UKI" $
        void $
          Util.quietReadProcess
            "ukify"
            [ "build",
              "--linux=" <> kernel,
              "--initrd=" <> initrd,
              "--cmdline=" <> cmdline,
              "--output=" <> x
            ]
            ""
    Nothing -> Util.fatal $ "No UKI path supplied for deployment " <> show (Dep.identifier deployment)

getPackageDB :: FilePath -> Config.Config -> Dep.Deployment -> IO ()
getPackageDB containerPath conf dep =
  Util.ioOrDie "Fetching package database" $
    Util.ensureDirExists
      ( Legacy.treeRootDir conf (Dep.identifier dep)
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

setDefaultBootEntry :: Int -> IO ()
setDefaultBootEntry dep =
  findExecutable "bootctl"
    >>= maybe
      (return ())
      ( \_ ->
          catch
            ( void $
                Util.quietReadProcess
                  "bootctl"
                  ["set-default", "*" <> show dep <> "*"]
                  ""
            )
            (\e -> let err = show (e :: IOException) in hPutStrLn stderr $ "Failed setting default boot entry: " <> err)
      )
