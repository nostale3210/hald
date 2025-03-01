module Main.Space where

import Control.Exception (IOException, catch)
import Control.Monad (when)
import Data.List (sort)
import Main.Config qualified as Config
import Main.Deployment qualified as Dep
import Main.Util qualified as Util
import System.Directory

rmDep :: Dep.Deployment -> Config.Config -> IO ()
rmDep deployment conf = do
  Util.printInfo
    ("Removing deployment " <> show (Dep.identifier deployment) <> "...")
    (Config.interactive conf)

  let depId = Dep.identifier deployment
  tbRmDep <-
    Dep.getDeployment
      depId
      (Config.rootDir conf)
      (Config.haldPath conf)
      (Config.bootPath conf)
  let bootComponents = Dep.bootComponents tbRmDep
  bootPathNoComps <- Util.pathExists (Config.bootPath conf <> "/" <> show depId)
  let tbRmBootComponents =
        if bootPathNoComps
          then
            Dep.BootComponents
              { Dep.bootDir = Just (Config.bootPath conf <> "/" <> show depId),
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
            Util.printInfo ("Couldn't remove " <> show ident <> " " <> component <> "; " <> err) False
        )
    Nothing ->
      Util.printInfo
        ("Deployment " <> show ident <> ": No associated " <> component <> ".")
        False

rmDeps :: Int -> [Int] -> Config.Config -> IO ()
rmDeps keepDeps deployments conf
  | keepDeps < length deployments =
      case sort deployments of
        [] -> return ()
        x : xs -> do
          tbRmDep <-
            Dep.getDeployment
              x
              (Config.rootDir conf)
              (Config.haldPath conf)
              (Config.bootPath conf)
          rmDep tbRmDep conf
          rmDeps keepDeps xs conf
  | otherwise = return ()

gcBroken :: [Int] -> Config.Config -> IO ()
gcBroken depIds conf = do
  case depIds of
    [] -> return ()
    x : xs -> do
      checkDep x conf
      gcBroken xs conf

checkDep :: Int -> Config.Config -> IO ()
checkDep depId conf = do
  dep <- Dep.getDeployment depId (Config.rootDir conf) (Config.haldPath conf) (Config.bootPath conf)
  let bootComps = Dep.bootComponents dep
  rootDirExists <- componentPresent $ Dep.rootDir dep
  bootDirExists <- componentPresent $ Dep.bootDir bootComps
  bootEntryExists <- componentPresent $ Dep.bootEntry bootComps
  lockfileExists <- componentPresent $ Dep.lockfile dep
  idFileExists <- Util.pathExists $ Config.haldPath conf <> "/" <> show depId <> "/usr/.ald_dep"
  if rootDirExists
    && bootDirExists
    && bootEntryExists
    && lockfileExists
    && idFileExists
    then do
      idFileContent <- readFile $ Config.haldPath conf <> "/" <> show depId <> "/usr/.ald_dep"
      let savedId = read (head $ lines idFileContent) :: Int
      when
        (savedId /= depId)
        $ catch
          ( writeFile
              (Config.haldPath conf <> "/" <> show depId <> "/usr/.ald_dep")
              (show depId)
          )
          ( \e -> do
              let err = show (e :: IOException)
              Util.printInfo
                ("Couldn't fix incorrect deployment id " <> show savedId <> "; " <> err)
                (Config.interactive conf)
          )
    else
      if Dep.rootDir dep == Just "/usr"
        then return ()
        else do
          Util.printInfo
            ("Collecting broken deployment " <> show depId <> "...")
            (Config.interactive conf)
          rmDep dep conf

componentPresent :: Maybe FilePath -> IO Bool
componentPresent compPath =
  case compPath of
    Just _ -> return True
    Nothing -> return False
