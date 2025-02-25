module Main.Create where

import Control.Exception (IOException, catch)
import Control.Monad (when)
import Data.List (nub)
import Main.Config qualified as Config
import Main.Container qualified as Cont
import Main.Deployment qualified as Dep
import Main.Fail qualified as Fail
import Main.Space qualified as Space
import Main.Util qualified as Util
import System.Directory
import System.Process

createSkeleton :: Int -> IO ()
createSkeleton depId = do
  Util.ensureDirExists (Config.haldPath <> "/" <> show depId)
  Util.ensureDirExists (Config.bootPath <> "/" <> show depId)

createBootEntry :: Int -> IO ()
createBootEntry depId = do
  Util.ensureDirExists $ Config.bootPath <> "/loader/entries"
  bootTemplateExists <- Util.pathExists (Config.haldPath <> "/boot.conf")
  if bootTemplateExists
    then do
      templateContent <-
        catch
          ( readFile $
              Config.haldPath <> "/boot.conf"
          )
          ( \e -> do
              let err = show (e :: IOException)
              deployment <- Dep.getDeployment depId
              Fail.failAndCleanup err deployment
              return ""
          )
      catch
        ( writeFile
            (Config.bootPath <> "/loader/entries/" <> show depId <> ".conf")
            (generateBootEntry depId templateContent)
        )
        ( \e -> do
            let err = show (e :: IOException)
            putStrLn "Couldn't create boot entry!"
            deployment <- Dep.getDeployment depId
            Fail.failAndCleanup err deployment
        )
    else
      error ("No boot entry template found, make sure it exists at " <> Config.haldPath <> "/boot.conf")

generateBootEntry :: Int -> String -> String
generateBootEntry depId = Util.replaceString "INSERT_DEPLOYMENT" (show depId)

createNewDeployment :: Dep.Deployment -> IO ()
createNewDeployment deployment = do
  allDeps <- Dep.getDeploymentsInt
  Space.gcBroken allDeps
  putStrLn $ "Creating deployment " <> show (Dep.identifier deployment) <> "..."
  containerMount <- Cont.mountContainer "ald-root" Config.containerUri
  Cont.syncImage containerMount
  Cont.umountContainer "ald-root"
  syncSystemConfig Config.discardState $ Dep.identifier deployment
  createSkeleton $ Dep.identifier deployment
  hardlinkDep deployment
  Cont.rmContainer "ald-root"
  placeBootFiles deployment
  createBootEntry $ Dep.identifier deployment
  Util.relabelSeLinuxPath
    (Config.haldPath <> "/" <> show (Dep.identifier deployment))
    ( Config.haldPath
        <> "/"
        <> show (Dep.identifier deployment)
        <> "/etc/selinux/targeted/contexts/files/file_contexts"
    )
    (Config.haldPath <> "/" <> show (Dep.identifier deployment))

syncSystemConfig :: Bool -> Int -> IO ()
syncSystemConfig dropState depId = do
  putStrLn $ "Syncing system config... (Dropping state: " <> show dropState <> ")"
  if dropState
    then syncMinimumState
    else syncState depId
  catch
    ( callCommand
        ( "podman cp ald-root:/etc/passwd "
            <> Config.haldPath
            <> " && podman cp ald-root:/etc/shadow "
            <> Config.haldPath
        )
    )
    ( \e -> do
        let err = show (e :: IOException)
        putStrLn "Couldn't sync system config!"
        Fail.failAndCleanup err $ Dep.createDeployment [depId]
    )
  mergeFiles "/etc/passwd" (Config.haldPath <> "/passwd") (Config.haldPath <> "/image/etc/passwd")
  mergeFiles "/etc/shadow" (Config.haldPath <> "/shadow") (Config.haldPath <> "/image/etc/shadow")
  removeTmpFile $ Config.haldPath <> "/passwd"
  removeTmpFile $ Config.haldPath <> "/shadow"

removeTmpFile :: FilePath -> IO ()
removeTmpFile file = do
  fileExists <- Util.pathExists file
  when fileExists $
    catch
      ( removeFile file
      )
      ( \e -> do
          let err = show (e :: IOException)
          putStrLn $ "Couldn't remove temporary files; " <> err
      )

mergeFiles :: FilePath -> FilePath -> FilePath -> IO ()
mergeFiles inputA inputB outputFile = do
  contentA <-
    catch
      (readFile inputA)
      ( \e -> do
          let err = show (e :: IOException)
          putStrLn "Failed to merge files!"
          Fail.failAndCleanup err $ Dep.createDeployment [-2]
          return ""
      )
  contentB <-
    catch
      (readFile inputB)
      ( \e -> do
          let err = show (e :: IOException)
          putStrLn "Failed to merge files!"
          Fail.failAndCleanup err $ Dep.createDeployment [-2]
          return ""
      )
  let output = unlines . nub . lines $ contentA <> contentB
  catch
    (writeFile outputFile output)
    ( \e -> do
        let err = show (e :: IOException)
        putStrLn "Failed to merge files!"
        Fail.failAndCleanup err $ Dep.createDeployment [-2]
    )

syncMinimumState :: IO ()
syncMinimumState =
  syncSingleFile
    ( words
        ( "/etc/fstab /etc/crypttab /etc/locale.conf /etc/localtime "
            <> "/etc/adjtime /etc/sudoers.d /etc/group /etc/gshadow /etc/subgid /etc/subuid "
            <> "/etc/NetworkManager/system-connections /etc/vconsole.conf /etc/pki "
            <> "/etc/firewalld /etc/environment /etc/hostname "
            <> "/etc/X11/xorg.conf.d/00-keyboard.conf /etc/sudoers /etc/ald"
        )
    )
    (Config.haldPath <> "/image/")

syncState :: Int -> IO ()
syncState depId = do
  let syncCmd = "cp -rfa --parents \"$@\" " <> Config.haldPath <> "/image/"
      statA = "xargs -I{} -P\"$((\"$(nproc --all)\"/2))\" stat --printf \"%Y\\t%n\\0\" {} 2>/dev/null | "
      xBashC = "xargs -0 -I{} -P\"$((\"$(nproc --all)\"/2))\" "
      testTsD = "bash -c 'test \"$(echo {} | cut -d\" \" -f1)\" == 0 || echo {}' | cut -d\" \" -f2 | "
      xargsSync = "xargs -n1 bash -c '" <> syncCmd <> "' _"
  catch
    ( callCommand
        ( "find /etc ! -type d | "
            <> statA
            <> xBashC
            <> testTsD
            <> xargsSync
        )
    )
    ( \e -> do
        let err = show (e :: IOException)
        deployment <- Dep.getDeployment depId
        Fail.failAndCleanup err deployment
    )
  putStrLn "Syncing essential files..."
  syncMinimumState

syncSingleFile :: [FilePath] -> FilePath -> IO ()
syncSingleFile files destination =
  case files of
    [] -> return ()
    x : xs -> do
      catch
        ( callCommand
            ( "cp -rfa --parents "
                <> x
                <> " "
                <> destination
            )
        )
        ( \e -> do
            let _ = show (e :: IOException)
            putStrLn $ "File " <> x <> " couldn't be synchronized."
        )
      syncSingleFile xs destination

hardlinkDep :: Dep.Deployment -> IO ()
hardlinkDep deployment = do
  putStrLn "Creating hardlinks to new deployment..."
  catch
    ( callCommand
        ("podman cp -a ald-root:/files " <> Config.haldPath <> "/." <> show (Dep.identifier deployment))
    )
    ( \e -> do
        let err = show (e :: IOException)
        Fail.failAndCleanup err deployment
    )
  Util.ensureDirExists $ Config.haldPath <> "/" <> show (Dep.identifier deployment) <> "/usr"
  catch
    ( callCommand
        ( "rsync -aHlx --link-dest=\"../../image/usr/\" "
            <> Config.haldPath
            <> "/image/usr/ "
            <> Config.haldPath
            <> "/"
            <> show (Dep.identifier deployment)
            <> "/usr/"
        )
    )
    ( \e -> do
        let err = show (e :: IOException)
        Fail.failAndCleanup err deployment
    )
  catch
    ( writeFile
        ( Config.haldPath
            <> "/"
            <> show (Dep.identifier deployment)
            <> "/usr/.ald_dep"
        )
        (show (Dep.identifier deployment))
    )
    ( \e -> do
        let err = show (e :: IOException)
        Fail.failAndCleanup err deployment
    )
  catch
    ( callCommand
        ( "rsync -aHlx "
            <> Config.haldPath
            <> "/image/etc "
            <> Config.haldPath
            <> "/"
            <> show (Dep.identifier deployment)
        )
    )
    ( \e -> do
        let err = show (e :: IOException)
        Fail.failAndCleanup err deployment
    )

placeBootFiles :: Dep.Deployment -> IO ()
placeBootFiles deployment = do
  putStrLn "Placing kernel and initramfs"
  kernel <- modulePathSearch "vmlinuz"
  initrd <- modulePathSearch "initramfs.img"
  let bootComps = Dep.bootComponents deployment
  case Dep.bootDir bootComps of
    Just x -> do
      copyFile (head kernel) (x <> "/vmlinuz")
      copyFile (head initrd) (x <> "/initramfs.img")
    Nothing -> Fail.failAndCleanup "Couldn't place place boot components." deployment
  where
    modulePathSearch =
      Util.recursiveFileSearch
        ( Config.haldPath
            <> "/"
            <> show (Dep.identifier deployment)
            <> "/usr/lib/modules"
        )
