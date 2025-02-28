module Main.Space where

import Control.Exception (IOException, catch)
import Control.Monad (when)
import Data.List (sort)
import Main.Deployment qualified as Dep
import Main.Util qualified as Util
import System.Directory

rmDep :: Dep.Deployment -> FilePath -> FilePath -> FilePath -> IO ()
rmDep deployment root hp bp = do
  let depId = Dep.identifier deployment
  putStrLn $ "Removing deployment " <> show depId <> "..."
  tbRmDep <- Dep.getDeployment depId root hp bp
  let bootComponents = Dep.bootComponents tbRmDep
  bootPathNoComps <- Util.pathExists (bp <> "/" <> show depId)
  let tbRmBootComponents =
        if bootPathNoComps
          then
            Dep.BootComponents
              { Dep.bootDir = Just (bp <> "/" <> show depId),
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

rmDeps :: Int -> [Int] -> FilePath -> FilePath -> FilePath -> IO ()
rmDeps keepDeps deployments root hp bp
  | keepDeps < length deployments =
      case sort deployments of
        [] -> return ()
        x : xs -> do
          tbRmDep <- Dep.getDeployment x root hp bp
          rmDep tbRmDep root hp bp
          rmDeps keepDeps xs root hp bp
  | otherwise = return ()

gcBroken :: [Int] -> FilePath -> FilePath -> FilePath -> IO ()
gcBroken depIds root hp bp = do
  case depIds of
    [] -> return ()
    x : xs -> do
      checkDep x root hp bp
      gcBroken xs root hp bp

checkDep :: Int -> FilePath -> FilePath -> FilePath -> IO ()
checkDep depId root hp bp = do
  dep <- Dep.getDeployment depId root hp bp
  let bootComps = Dep.bootComponents dep
  rootDirExists <- componentPresent $ Dep.rootDir dep
  bootDirExists <- componentPresent $ Dep.bootDir bootComps
  bootEntryExists <- componentPresent $ Dep.bootEntry bootComps
  lockfileExists <- componentPresent $ Dep.lockfile dep
  idFileExists <- Util.pathExists $ hp <> "/" <> show depId <> "/usr/.ald_dep"
  if rootDirExists
    && bootDirExists
    && bootEntryExists
    && lockfileExists
    && idFileExists
    then do
      idFileContent <- readFile $ hp <> "/" <> show depId <> "/usr/.ald_dep"
      let savedId = read (head $ lines idFileContent) :: Int
      when
        (savedId /= depId)
        $ catch
          ( writeFile
              (hp <> "/" <> show depId <> "/usr/.ald_dep")
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
          rmDep dep root hp bp

componentPresent :: Maybe FilePath -> IO Bool
componentPresent compPath =
  case compPath of
    Just _ -> return True
    Nothing -> return False
