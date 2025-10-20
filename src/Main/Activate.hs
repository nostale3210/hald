module Main.Activate where

import Control.Exception (IOException, catch)
import Control.Monad (when)
import Main.Deployment qualified as Dep
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

activateNewRoot :: FilePath -> Dep.Deployment -> FilePath -> IO ()
activateNewRoot root newDep hp = do
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
  newRoot <- getNewRoot newDep
  when (oldId /= newId) $ do
    Util.ensureDirExists $ root <> "/usr"
    Util.ensureDirExists $ root <> "/etc"
    blockSignals signalsToBlock
    exchPaths (root <> "/usr") (newRoot <> "/usr")
    exchPaths (root <> "/etc") (newRoot <> "/etc")
    movePath newRoot (hp <> "/" <> show oldId)
    unblockSignals signalsToBlock
