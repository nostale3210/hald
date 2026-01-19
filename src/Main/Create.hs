module Main.Create where

import Control.Exception (IOException, catch)
import Control.Monad (when)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Main.Config qualified as Config
import Main.Deployment qualified as Dep
import Main.Util qualified as Util
import System.Directory
import System.FilePath (takeDirectory)
import System.Posix.Signals (raiseSignal, sigTERM)
import System.Process

createSkeleton :: Int -> Config.Config -> Bool -> IO ()
createSkeleton depId conf uki =
  Util.ensureDirExists (Config.haldPath conf <> "/" <> show depId)
    >> Util.createSymlink "usr/lib" (Config.haldPath conf <> "/" <> show depId <> "/lib")
    >> if uki
      then Util.ensureDirExists (Config.ukiPath conf)
      else Util.ensureDirExists (Config.bootPath conf <> "/" <> show depId)

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
               in putStrLn err
                    >> raiseSignal sigTERM
                    >> return ""
          )
      catch
        ( writeFile
            (Config.bootPath conf <> "/loader/entries/" <> show depId <> ".conf")
            (generateBootEntry depId templateContent)
        )
        ( \e ->
            let err = show (e :: IOException)
             in Util.printInfo ("Couldn't create boot entry!\n" <> err) (Config.interactive conf)
                  >> raiseSignal sigTERM
        )
    else
      error
        ( "No boot entry template found, make sure it exists at "
            <> Config.configPath conf
            <> "/boot.conf"
        )
        >> raiseSignal sigTERM

generateBootEntry :: Int -> String -> String
generateBootEntry depId = Util.replaceString "INSERT_DEPLOYMENT" (show depId)

syncSystemConfig :: Bool -> Config.Config -> IO ()
syncSystemConfig dropState conf = do
  if dropState
    then syncMinimumState (Config.haldPath conf)
    else syncState conf
  catch
    ( callCommand
        ( "podman cp ald-root:/etc/passwd "
            <> Config.haldPath conf
            <> " && podman cp ald-root:/etc/shadow "
            <> Config.haldPath conf
        )
    )
    ( \e ->
        let err = show (e :: IOException)
         in Util.printInfo ("Couldn't sync system config!\n" <> err) (Config.interactive conf)
              >> raiseSignal sigTERM
    )
  mergeFiles "/etc/passwd" (Config.haldPath conf <> "/passwd") (Config.haldPath conf <> "/image/etc/passwd")
    >> removeTmpFile (Config.haldPath conf <> "/passwd")
  mergeFiles "/etc/shadow" (Config.haldPath conf <> "/shadow") (Config.haldPath conf <> "/image/etc/shadow")
    >> removeTmpFile (Config.haldPath conf <> "/shadow")

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
           in Util.printInfo "Failed to merge files!" False
                >> raiseSignal sigTERM
                >> return ""
      )
  contentB <-
    catch
      (readFile inputB)
      ( \e ->
          let _ = show (e :: IOException)
           in Util.printInfo "Failed to merge files!" False
                >> raiseSignal sigTERM
                >> return ""
      )
  let output = unlines . nub . lines $ contentA <> contentB
  catch
    (writeFile outputFile output)
    ( \e ->
        let _ = show (e :: IOException)
         in Util.printInfo "Failed to merge files!" False
              >> raiseSignal sigTERM
    )

syncMinimumState :: FilePath -> IO ()
syncMinimumState hp =
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
    (hp <> "/image/")

syncState :: Config.Config -> IO ()
syncState conf =
  let syncCmd = "rsync -aRI \"$@\" " <> Config.haldPath conf <> "/image/ >/dev/null 2>&1 || :"
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
             in putStrLn err
                  >> raiseSignal sigTERM
        )
        >> syncMinimumState (Config.haldPath conf)

syncSingleFile :: [FilePath] -> FilePath -> IO ()
syncSingleFile files destination
  | null files = return ()
  | otherwise =
      mapM_
        ( \y ->
            catch
              ( callCommand
                  ( "rsync -aRI  "
                      <> y
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
        )
        files

hardlinkDep :: Dep.Deployment -> FilePath -> IO ()
hardlinkDep deployment hp = do
  catch
    ( callCommand
        ( "podman cp -a ald-root:/files "
            <> hp
            <> "/."
            <> show (Dep.identifier deployment)
            <> " >/dev/null 2>&1"
        )
    )
    ( \e ->
        let _ = show (e :: IOException)
         in catch
              (writeFile (hp <> "/." <> show (Dep.identifier deployment)) "")
              (\e2 -> let _ = show (e2 :: IOException) in raiseSignal sigTERM)
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
    (\e -> let _ = show (e :: IOException) in raiseSignal sigTERM)
  catch
    ( writeFile
        ( hp
            <> "/"
            <> show (Dep.identifier deployment)
            <> "/usr/.ald_dep"
        )
        (show (Dep.identifier deployment))
    )
    (\e -> let _ = show (e :: IOException) in raiseSignal sigTERM)
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
    (\e -> let _ = show (e :: IOException) in raiseSignal sigTERM)

modulePathSearch :: Config.Config -> Dep.Deployment -> FilePath -> IO FilePath
modulePathSearch conf deployment target =
  Util.recursiveFileSearch
    ( Config.haldPath conf
        <> "/"
        <> show (Dep.identifier deployment)
        <> "/usr/lib/modules"
    )
    target
    >>= \path -> return $ head path

placeBootFiles :: Config.Config -> Dep.Deployment -> IO ()
placeBootFiles conf deployment = do
  kernel <- modulePathSearch conf deployment "vmlinuz"
  initrd <- modulePathSearch conf deployment "initramfs.img"
  let bootComps = Dep.bootComponents deployment
  case Dep.bootDir bootComps of
    Just x -> do
      copyFile kernel (x <> "/vmlinuz")
      copyFile initrd (x <> "/initramfs.img")
    Nothing -> raiseSignal sigTERM

installUki :: Config.Config -> Dep.Deployment -> IO ()
installUki conf deployment = do
  kernel <- modulePathSearch conf deployment "vmlinuz"
  initrd <- modulePathSearch conf deployment "initramfs.img"
  templCmdline <-
    catch
      ( readFile $
          Config.configPath conf <> "/cmdline"
      )
      ( \e ->
          let err = show (e :: IOException)
           in putStrLn err
                >> raiseSignal sigTERM
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
        ( \e -> let _ = show (e :: IOException) in raiseSignal sigTERM
        )
    Nothing -> raiseSignal sigTERM

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
         in putStrLn "Failed fetching package db!"
              >> raiseSignal sigTERM
    )
