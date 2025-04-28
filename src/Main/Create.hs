module Main.Create where

import Control.Exception (IOException, catch)
import Control.Monad (when)
import Data.List (nub)
import Main.Config qualified as Config
import Main.Deployment qualified as Dep
import Main.Util qualified as Util
import System.Directory
import System.Posix.Signals (raiseSignal, sigTERM)
import System.Process

createSkeleton :: Int -> Config.Config -> IO ()
createSkeleton depId conf =
  Util.ensureDirExists (Config.haldPath conf <> "/" <> show depId)
    >> Util.ensureDirExists (Config.bootPath conf <> "/" <> show depId)

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
            <> Config.haldPath conf
            <> "/boot.conf"
        )

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
            <> "/etc/X11/xorg.conf.d/00-keyboard.conf /etc/sudoers /etc/hald"
        )
    )
    (hp <> "/image/")

syncState :: Config.Config -> IO ()
syncState conf =
  let syncCmd = "cp -rfa --parents \"$@\" " <> Config.haldPath conf <> "/image/"
      statA = "xargs -I{} -P\"$((\"$(nproc --all)\"/2))\" stat --printf \"%Y\\t%n\\0\" {} 2>/dev/null | "
      xBashC = "xargs -0 -I{} -P\"$((\"$(nproc --all)\"/2))\" "
      testTsD = "bash -c 'test \"$(echo {} | cut -d\" \" -f1)\" == 0 || echo {}' | cut -d\" \" -f2 | "
      xargsSync = "xargs -n1 bash -c '" <> syncCmd <> "' _"
   in catch
        ( callCommand
            ( "find /etc ! -type d | "
                <> statA
                <> xBashC
                <> testTsD
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
syncSingleFile files destination =
  case files of
    [] -> return ()
    x : _ ->
      mapM_
        ( \y ->
            catch
              ( callCommand
                  ( "cp -rfa --parents "
                      <> y
                      <> " "
                      <> destination
                  )
              )
              ( \e ->
                  let _ = show (e :: IOException)
                   in Util.printInfo ("File " <> x <> " couldn't be synchronized.") False
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
            <> " &>/dev/null"
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

placeBootFiles :: Dep.Deployment -> FilePath -> IO ()
placeBootFiles deployment hp = do
  kernel <- modulePathSearch "vmlinuz"
  initrd <- modulePathSearch "initramfs.img"
  let bootComps = Dep.bootComponents deployment
  case Dep.bootDir bootComps of
    Just x -> do
      copyFile (head kernel) (x <> "/vmlinuz")
      copyFile (head initrd) (x <> "/initramfs.img")
    Nothing -> raiseSignal sigTERM
  where
    modulePathSearch =
      Util.recursiveFileSearch
        ( hp
            <> "/"
            <> show (Dep.identifier deployment)
            <> "/usr/lib/modules"
        )
