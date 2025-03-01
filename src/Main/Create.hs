module Main.Create where

import Control.Exception (IOException, catch)
import Control.Monad (when)
import Data.List (nub)
import Main.Config qualified as Config
import Main.Deployment qualified as Dep
import Main.Fail qualified as Fail
import Main.Util qualified as Util
import System.Directory
import System.Process

createSkeleton :: Int -> Config.Config -> IO ()
createSkeleton depId conf = do
  Util.ensureDirExists (Config.haldPath conf <> "/" <> show depId)
  Util.ensureDirExists (Config.bootPath conf <> "/" <> show depId)

createBootEntry :: Int -> Config.Config -> IO ()
createBootEntry depId conf = do
  Util.ensureDirExists $ Config.bootPath conf <> "/loader/entries"
  bootTemplateExists <- Util.pathExists (Config.haldPath conf <> "/boot.conf")
  if bootTemplateExists
    then do
      templateContent <-
        catch
          ( readFile $
              Config.haldPath conf <> "/boot.conf"
          )
          ( \e -> do
              let err = show (e :: IOException)
              deployment <-
                Dep.getDeployment
                  depId
                  (Config.rootDir conf)
                  (Config.haldPath conf)
                  (Config.bootPath conf)
              Fail.failAndCleanup err deployment
              return ""
          )
      catch
        ( writeFile
            (Config.bootPath conf <> "/loader/entries/" <> show depId <> ".conf")
            (generateBootEntry depId templateContent)
        )
        ( \e -> do
            let err = show (e :: IOException)
            Util.printInfo "Couldn't create boot entry!" (Config.interactive conf)
            deployment <-
              Dep.getDeployment
                depId
                (Config.rootDir conf)
                (Config.haldPath conf)
                (Config.bootPath conf)
            Fail.failAndCleanup err deployment
        )
    else
      error
        ( "No boot entry template found, make sure it exists at "
            <> Config.haldPath conf
            <> "/boot.conf"
        )

generateBootEntry :: Int -> String -> String
generateBootEntry depId = Util.replaceString "INSERT_DEPLOYMENT" (show depId)

syncSystemConfig :: Bool -> Int -> Config.Config -> IO ()
syncSystemConfig dropState depId conf = do
  if dropState
    then syncMinimumState (Config.haldPath conf)
    else syncState depId conf
  catch
    ( callCommand
        ( "podman cp ald-root:/etc/passwd "
            <> Config.haldPath conf
            <> " && podman cp ald-root:/etc/shadow "
            <> Config.haldPath conf
        )
    )
    ( \e -> do
        let err = show (e :: IOException)
        Util.printInfo "Couldn't sync system config!" (Config.interactive conf)
        Fail.failAndCleanup err $
          Dep.createDeployment
            [depId]
            (Config.haldPath conf)
            (Config.bootPath conf)
    )
  mergeFiles "/etc/passwd" (Config.haldPath conf <> "/passwd") (Config.haldPath conf <> "/image/etc/passwd")
  mergeFiles "/etc/shadow" (Config.haldPath conf <> "/shadow") (Config.haldPath conf <> "/image/etc/shadow")
  removeTmpFile $ Config.haldPath conf <> "/passwd"
  removeTmpFile $ Config.haldPath conf <> "/shadow"

removeTmpFile :: FilePath -> IO ()
removeTmpFile file = do
  fileExists <- Util.pathExists file
  when fileExists $
    catch
      ( removeFile file
      )
      ( \e -> do
          let err = show (e :: IOException)
          Util.printInfo ("Couldn't remove temporary files; " <> err) False
      )

mergeFiles :: FilePath -> FilePath -> FilePath -> IO ()
mergeFiles inputA inputB outputFile = do
  contentA <-
    catch
      (readFile inputA)
      ( \e -> do
          let err = show (e :: IOException)
          Util.printInfo "Failed to merge files!" False
          Fail.failAndCleanup err Dep.dummyDeployment
          return ""
      )
  contentB <-
    catch
      (readFile inputB)
      ( \e -> do
          let err = show (e :: IOException)
          Util.printInfo "Failed to merge files!" False
          Fail.failAndCleanup err Dep.dummyDeployment
          return ""
      )
  let output = unlines . nub . lines $ contentA <> contentB
  catch
    (writeFile outputFile output)
    ( \e -> do
        let err = show (e :: IOException)
        Util.printInfo "Failed to merge files!" False
        Fail.failAndCleanup err Dep.dummyDeployment
    )

syncMinimumState :: FilePath -> IO ()
syncMinimumState hp =
  syncSingleFile
    ( words
        ( "/etc/fstab /etc/crypttab /etc/locale.conf /etc/localtime "
            <> "/etc/adjtime /etc/sudoers.d /etc/group /etc/gshadow /etc/subgid /etc/subuid "
            <> "/etc/NetworkManager/system-connections /etc/vconsole.conf /etc/pki "
            <> "/etc/firewalld /etc/environment /etc/hostname "
            <> "/etc/X11/xorg.conf.d/00-keyboard.conf /etc/sudoers /etc/ald"
        )
    )
    (hp <> "/image/")

syncState :: Int -> Config.Config -> IO ()
syncState depId conf = do
  let syncCmd = "cp -rfa --parents \"$@\" " <> Config.haldPath conf <> "/image/"
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
        deployment <-
          Dep.getDeployment
            depId
            (Config.rootDir conf)
            (Config.haldPath conf)
            (Config.bootPath conf)
        Fail.failAndCleanup err deployment
    )
  syncMinimumState (Config.haldPath conf)

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
            Util.printInfo ("File " <> x <> " couldn't be synchronized.") False
        )
      syncSingleFile xs destination

hardlinkDep :: Dep.Deployment -> FilePath -> IO ()
hardlinkDep deployment hp = do
  catch
    ( callCommand
        ("podman cp -a ald-root:/files " <> hp <> "/." <> show (Dep.identifier deployment))
    )
    ( \e -> do
        let err = show (e :: IOException)
        Fail.failAndCleanup err deployment
    )
  Util.ensureDirExists $ hp <> "/" <> show (Dep.identifier deployment) <> "/usr"
  catch
    ( callCommand
        ( "rsync -aHlx --link-dest=\"../../image/usr/\" "
            <> hp
            <> "/image/usr/ "
            <> hp
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
        ( hp
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
            <> hp
            <> "/image/etc "
            <> hp
            <> "/"
            <> show (Dep.identifier deployment)
        )
    )
    ( \e -> do
        let err = show (e :: IOException)
        Fail.failAndCleanup err deployment
    )

placeBootFiles :: Dep.Deployment -> FilePath -> IO ()
placeBootFiles deployment hp = do
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
        ( hp
            <> "/"
            <> show (Dep.identifier deployment)
            <> "/usr/lib/modules"
        )
