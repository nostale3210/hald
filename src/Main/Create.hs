module Main.Create where

import Control.Exception (IOException, catch)
import Control.Monad (when)
import Data.List (nub)
import Main.Deployment qualified as Dep
import Main.Fail qualified as Fail
import Main.Util qualified as Util
import System.Directory
import System.Process

createSkeleton :: Int -> FilePath -> FilePath -> IO ()
createSkeleton depId hp bp = do
  Util.ensureDirExists (hp <> "/" <> show depId)
  Util.ensureDirExists (bp <> "/" <> show depId)

createBootEntry :: Int -> FilePath -> FilePath -> FilePath -> IO ()
createBootEntry depId root hp bp = do
  Util.ensureDirExists $ bp <> "/loader/entries"
  bootTemplateExists <- Util.pathExists (hp <> "/boot.conf")
  if bootTemplateExists
    then do
      templateContent <-
        catch
          ( readFile $
              hp <> "/boot.conf"
          )
          ( \e -> do
              let err = show (e :: IOException)
              deployment <- Dep.getDeployment depId root hp bp
              Fail.failAndCleanup err deployment
              return ""
          )
      catch
        ( writeFile
            (bp <> "/loader/entries/" <> show depId <> ".conf")
            (generateBootEntry depId templateContent)
        )
        ( \e -> do
            let err = show (e :: IOException)
            putStrLn "Couldn't create boot entry!"
            deployment <- Dep.getDeployment depId root hp bp
            Fail.failAndCleanup err deployment
        )
    else
      error ("No boot entry template found, make sure it exists at " <> hp <> "/boot.conf")

generateBootEntry :: Int -> String -> String
generateBootEntry depId = Util.replaceString "INSERT_DEPLOYMENT" (show depId)

syncSystemConfig :: Bool -> Int -> FilePath -> FilePath -> FilePath -> IO ()
syncSystemConfig dropState depId root hp bp = do
  putStrLn $ "Syncing system config... (Dropping state: " <> show dropState <> ")"
  if dropState
    then syncMinimumState hp
    else syncState depId root hp bp
  catch
    ( callCommand
        ( "podman cp ald-root:/etc/passwd "
            <> hp
            <> " && podman cp ald-root:/etc/shadow "
            <> hp
        )
    )
    ( \e -> do
        let err = show (e :: IOException)
        putStrLn "Couldn't sync system config!"
        Fail.failAndCleanup err $ Dep.createDeployment [depId] hp bp
    )
  mergeFiles "/etc/passwd" (hp <> "/passwd") (hp <> "/image/etc/passwd")
  mergeFiles "/etc/shadow" (hp <> "/shadow") (hp <> "/image/etc/shadow")
  removeTmpFile $ hp <> "/passwd"
  removeTmpFile $ hp <> "/shadow"

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
          Fail.failAndCleanup err Dep.dummyDeployment
          return ""
      )
  contentB <-
    catch
      (readFile inputB)
      ( \e -> do
          let err = show (e :: IOException)
          putStrLn "Failed to merge files!"
          Fail.failAndCleanup err Dep.dummyDeployment
          return ""
      )
  let output = unlines . nub . lines $ contentA <> contentB
  catch
    (writeFile outputFile output)
    ( \e -> do
        let err = show (e :: IOException)
        putStrLn "Failed to merge files!"
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

syncState :: Int -> FilePath -> FilePath -> FilePath -> IO ()
syncState depId root hp bp = do
  let syncCmd = "cp -rfa --parents \"$@\" " <> hp <> "/image/"
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
        deployment <- Dep.getDeployment depId root hp bp
        Fail.failAndCleanup err deployment
    )
  putStrLn "Syncing essential files..."
  syncMinimumState hp

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

hardlinkDep :: Dep.Deployment -> FilePath -> IO ()
hardlinkDep deployment hp = do
  putStrLn "Creating hardlinks to new deployment..."
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
        ( hp
            <> "/"
            <> show (Dep.identifier deployment)
            <> "/usr/lib/modules"
        )
