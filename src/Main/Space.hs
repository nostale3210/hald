module Main.Space where

import Control.Exception (IOException, catch)
import Control.Monad (when)
import Data.List (sort)
import Main.Config qualified as Config
import Main.Deployment qualified as Dep
import Main.Util qualified as Util
import System.Directory

rmDep :: Dep.Deployment -> IO ()
rmDep deployment = do
  let depId = Dep.identifier deployment
  putStrLn $ "Removing deployment " <> show depId <> "..."
  tbRmDep <- Dep.getDeployment depId
  let bootComponents = Dep.bootComponents tbRmDep
  bootPathNoComps <- Util.pathExists (Config.bootPath <> "/" <> show depId)
  let tbRmBootComponents =
        if bootPathNoComps
          then
            Dep.BootComponents
              { Dep.bootDir = Just (Config.bootPath <> "/" <> show depId),
                Dep.bootEntry = Dep.bootEntry bootComponents
              }
          else
            bootComponents
  rmComponent depId "root dir" (Dep.rootDir tbRmDep)
  rmComponent depId "boot dir" (Dep.bootDir tbRmBootComponents)
  rmComponent depId "boot entry" (Dep.bootEntry tbRmBootComponents)
  rmComponent depId "lockfile" (Dep.lockfile tbRmDep)

rmComponent :: Int -> String -> Maybe FilePath -> IO ()
rmComponent ident component path =
  case path of
    Just p ->
      catch
        (removePathForcibly p)
        ( \e -> do
            let err = show (e :: IOException)
            putStrLn ("Couldn't remove " <> show ident <> " " <> component <> "; " <> err)
        )
    Nothing -> putStrLn ("Deployment " <> show ident <> ": No associated " <> component <> ".")

rmDeps :: Int -> [Int] -> IO ()
rmDeps keepDeps deployments
  | keepDeps < length (sort deployments) =
      case deployments of
        [] -> return ()
        x : xs -> do
          tbRmDep <- Dep.getDeployment x

          rmDep tbRmDep
          rmDeps keepDeps xs
  | otherwise = putStrLn "No deployments to remove."

gcBroken :: [Int] -> IO ()
gcBroken depIds = do
  case depIds of
    [] -> return ()
    x : xs -> do
      checkDep x
      gcBroken xs

checkDep :: Int -> IO ()
checkDep depId = do
  dep <- Dep.getDeployment depId
  let bootComps = Dep.bootComponents dep
  rootDirExists <- componentPresent $ Dep.rootDir dep
  bootDirExists <- componentPresent $ Dep.bootDir bootComps
  bootEntryExists <- componentPresent $ Dep.bootEntry bootComps
  lockfileExists <- componentPresent $ Dep.lockfile dep
  idFileExists <- Util.pathExists $ Config.haldPath <> "/" <> show depId <> "/usr/.ald_dep"
  if rootDirExists
    && bootDirExists
    && bootEntryExists
    && lockfileExists
    && idFileExists
    then do
      idFileContent <- readFile $ Config.haldPath <> "/" <> show depId <> "/usr/.ald_dep"
      let savedId = read (head $ lines idFileContent) :: Int
      when
        (savedId /= depId)
        $ catch
          ( writeFile
              (Config.haldPath <> "/" <> show depId <> "/usr/.ald_dep")
              (show depId)
          )
          ( \e -> do
              let err = show (e :: IOException)
              putStrLn $ "Couldn't fix incorrect deployment id " <> show savedId <> "; " <> err
          )
    else
      if Dep.rootDir dep == Just "/usr"
        then return ()
        else do
          putStrLn $ "Collecting broken deployment " <> show depId <> "..."
          rmDep dep

componentPresent :: Maybe FilePath -> IO Bool
componentPresent compPath =
  case compPath of
    Just _ -> return True
    Nothing -> return False
