module Main.Activate where

import Control.Exception (IOException, catch)
import Control.Monad (when)
import Main.Deployment qualified as Dep
import Main.Lock qualified as Lock
import Main.Util qualified as Util
import System.Directory
import System.Posix.Signals (addSignal, blockSignals, emptySignalSet, raiseSignal, sigINT, sigTERM, unblockSignals)
import System.Process (callCommand)

getNewRoot :: Dep.Deployment -> IO FilePath
getNewRoot nextDep =
  case Dep.rootDir nextDep of
    Nothing -> raiseSignal sigTERM >> return "Defective"
    Just x -> return x

exchPaths :: FilePath -> FilePath -> IO ()
exchPaths oldPath newPath =
  catch
    ( callCommand
        ( "exch "
            <> oldPath
            <> " "
            <> newPath
        )
    )
    ( \e ->
        let err = show (e :: IOException)
         in putStrLn err
              >> raiseSignal sigTERM
    )

movePath :: FilePath -> FilePath -> IO ()
movePath oldPath newPath =
  catch
    (renameDirectory oldPath newPath)
    ( \e ->
        let err = show (e :: IOException)
         in putStrLn err
              >> raiseSignal sigTERM
    )

delegateMount :: FilePath -> FilePath -> IO ()
delegateMount fromPath toPath =
  catch
    (callCommand ("move-mount -db " <> fromPath <> " " <> toPath <> " &>/dev/null"))
    ( \e ->
        let err = show (e :: IOException)
         in putStrLn err
              >> raiseSignal sigTERM
    )

moveOMount :: FilePath -> FilePath -> FilePath -> IO ()
moveOMount hp fromPath toPath =
  catch
    ( callCommand
        ( "mount -t overlay overlay --make-private -o lowerdir="
            <> fromPath
            <> ":"
            <> (hp <> "/empty")
            <> " "
            <> fromPath
            <> " && move-mount -mb "
            <> fromPath
            <> " "
            <> toPath
            <> " &>/dev/null"
        )
    )
    ( \e ->
        let err = show (e :: IOException)
         in putStrLn err
              >> raiseSignal sigTERM
    )

usrOverlayMount :: FilePath -> FilePath -> FilePath -> IO ()
usrOverlayMount hp fromPath toPath =
  catch
    ( callCommand
        ( "mount -t overlay overlay --make-private -o lowerdir="
            <> fromPath
            <> ":"
            <> (hp <> "/empty")
            <> " "
            <> toPath
        )
    )
    ( \e ->
        let err = show (e :: IOException)
         in putStrLn err
              >> raiseSignal sigTERM
    )

privateMount :: FilePath -> IO ()
privateMount path =
  catch
    (callCommand ("mount --make-private " <> path))
    ( \e ->
        let err = show (e :: IOException)
         in putStrLn err
              >> raiseSignal sigTERM
    )

bindMount :: FilePath -> FilePath -> IO ()
bindMount fromPath toPath =
  catch
    (callCommand ("mount -o bind --make-private " <> fromPath <> " " <> toPath))
    ( \e ->
        let err = show (e :: IOException)
         in putStrLn err
              >> raiseSignal sigTERM
    )

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
  let oldId = read (head $ words idFileContent) :: Int
      newId = Dep.identifier newDep
      signalsToBlock = addSignal sigTERM . addSignal sigINT $ emptySignalSet
  usrMounted <- Util.isMountpoint $ root <> "/usr"
  etcMounted <- Util.isMountpoint $ root <> "/etc"
  newRoot <- getNewRoot newDep
  when (oldId /= newId) $ do
    Util.ensureDirExists $ root <> "/usr"
    Util.ensureDirExists $ root <> "/etc"
    privateMount hp
    blockSignals signalsToBlock
    if usrMounted
      then
        privateMount (root <> "/usr")
          >> moveOMount hp (newRoot <> "/usr") (root <> "/usr")
          >> Lock.umountDirForcibly Lock.Fl (root <> "/usr")
      else usrOverlayMount hp (newRoot <> "/usr") (root <> "/usr")
    if etcMounted
      then
        privateMount (root <> "/etc")
          >> delegateMount (newRoot <> "/etc") (root <> "/etc")
          >> Lock.umountDirForcibly Lock.Fl (root <> "/etc")
      else bindMount (newRoot <> "/etc") (root <> "/etc")
    unblockSignals signalsToBlock
