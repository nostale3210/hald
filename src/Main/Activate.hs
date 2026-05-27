module Main.Activate where

import Control.Exception (IOException, catch)
import Control.Monad (void, when)
import Main.Deployment qualified as Dep
import Main.Lock qualified as Lock
import Main.Util qualified as Util
import System.Posix.Signals (addSignal, blockSignals, emptySignalSet, sigINT, sigTERM, unblockSignals)
import System.Process (readProcess)

getNewRoot :: Dep.Deployment -> IO FilePath
getNewRoot nextDep =
  case Dep.rootDir nextDep of
    Nothing -> Util.fatalWith "nextDep doesn't supply rootDir" ""
    Just x -> return x

delegateMount :: FilePath -> FilePath -> IO ()
delegateMount fromPath toPath =
  catch
    (void $ Util.quietReadProcess "move-mount" ["-db", fromPath, toPath] "")
    (\e -> Util.fatal $ show (e :: IOException))

moveOMount :: FilePath -> FilePath -> IO ()
moveOMount fromPath toPath =
  catch
    (void $ Util.quietReadProcess "move-mount" ["-mb", fromPath, toPath] "")
    (\e -> Util.fatal $ show (e :: IOException))

usrOverlayMount :: FilePath -> FilePath -> FilePath -> IO ()
usrOverlayMount hp fromPath toPath =
  catch
    ( void $
        readProcess
          "mount"
          [ "-t",
            "overlay",
            "usr-root",
            "--make-private",
            "-o",
            "lowerdir=" <> fromPath <> ":" <> hp <> "/empty",
            toPath
          ]
          ""
    )
    (\e -> Util.fatal $ show (e :: IOException))

privateMount :: FilePath -> IO ()
privateMount path =
  catch
    (void $ readProcess "mount" ["--make-private", path] "")
    (\e -> Util.fatal $ show (e :: IOException))

bindMount :: FilePath -> FilePath -> IO ()
bindMount fromPath toPath =
  catch
    (void $ readProcess "mount" ["-o", "bind", "--make-private", fromPath, toPath] "")
    (\e -> Util.fatal $ show (e :: IOException))

activateNewRoot :: FilePath -> FilePath -> Dep.Deployment -> IO ()
activateNewRoot root hp newDep = do
  idFileContent <-
    catch
      (readFile (root <> "/usr/.ald_dep"))
      ( \e ->
          let err = show (e :: IOException)
           in Util.printInfo ("Couldn't read deployment ID; " <> err) False
                >> return "0"
      )
  let oldId = case reads (head $ words idFileContent) of [(n, "")] -> n; _ -> 0
      newId = Dep.identifier newDep
      signalsToBlock = addSignal sigTERM . addSignal sigINT $ emptySignalSet
  usrMounted <- Util.isMountpoint $ root <> "/usr"
  etcMounted <- Util.isMountpoint $ root <> "/etc"
  newRoot <- getNewRoot newDep
  when (oldId /= newId) $ do
    privateMount hp
    blockSignals signalsToBlock
    if usrMounted
      then
        privateMount (root <> "/usr")
          >> usrOverlayMount hp (newRoot <> "/usr") (newRoot <> "/usr")
          >> moveOMount (newRoot <> "/usr") (root <> "/usr")
          >> Lock.umountDirForcibly Lock.Fl (root <> "/usr")
      else usrOverlayMount hp (newRoot <> "/usr") (root <> "/usr")
    if etcMounted
      then
        privateMount (root <> "/etc")
          >> delegateMount (newRoot <> "/etc") (root <> "/etc")
          >> Lock.umountDirForcibly Lock.Fl (root <> "/etc")
      else bindMount (newRoot <> "/etc") (root <> "/etc")
    unblockSignals signalsToBlock
