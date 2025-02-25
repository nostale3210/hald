module Main.Activate where

import Control.Exception (IOException, catch)
import Control.Monad (when)
import Main.Deployment qualified as Dep
import Main.Fail qualified as Fail
import Main.Util qualified as Util
import System.Directory
import System.Process (callCommand)

getNewRoot :: Dep.Deployment -> IO FilePath
getNewRoot nextDep =
  case Dep.rootDir nextDep of
    Nothing -> do
      Fail.failAndCleanup
        ("Deployment " <> show (Dep.identifier nextDep) <> " is defective")
        Dep.dummyDeployment
      return "Defective"
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
    ( \e -> do
        let err = show (e :: IOException)
        Fail.failAndCleanup err Dep.dummyDeployment
    )

movePath :: FilePath -> FilePath -> IO ()
movePath oldPath newPath =
  catch
    (renameDirectory oldPath newPath)
    ( \e -> do
        let err = show (e :: IOException)
        Fail.failAndCleanup err Dep.dummyDeployment
    )

activateNewRoot :: FilePath -> Dep.Deployment -> FilePath -> IO ()
activateNewRoot root newDep hp = do
  idFileContent <-
    catch
      (readFile (root <> "/usr/.ald_dep"))
      ( \e -> do
          let err = show (e :: IOException)
          putStrLn $ "Couldn't read deployment ID; " <> err
          return "0"
      )
  let oldId = read (head $ words idFileContent) :: Int
      newId = Dep.identifier newDep
  newRoot <- getNewRoot newDep
  when (oldId /= newId) $ do
    Util.ensureDirExists $ root <> "/usr"
    Util.ensureDirExists $ root <> "/etc"
    exchPaths (root <> "/usr") (newRoot <> "/usr")
    exchPaths (root <> "/etc") (newRoot <> "/etc")
    movePath newRoot (hp <> "/" <> show oldId)
