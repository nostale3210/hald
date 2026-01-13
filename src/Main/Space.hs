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
  tbRmDep <- tbRmDep'
  let bootComponents = Dep.bootComponents tbRmDep
  bootPathNoComps <- Util.pathExists (Config.bootPath conf <> "/" <> show depId)
  let tbRmBootComponents =
        if bootPathNoComps
          then
            Dep.BootComponents
              { Dep.bootDir = Just (Config.bootPath conf <> "/" <> show depId),
                Dep.bootEntry = Dep.bootEntry bootComponents,
                Dep.ukiPath = Dep.ukiPath bootComponents
              }
          else
            bootComponents
  if Dep.rootDir tbRmDep /= Just "/usr"
    then
      mapM_
        (uncurry (rmComponent depId))
        [ ("root dir", Dep.rootDir tbRmDep),
          ("boot dir", Dep.bootDir tbRmBootComponents),
          ("boot entry", Dep.bootEntry tbRmBootComponents),
          ("lockfile", Dep.lockfile tbRmDep)
        ]
    else
      Util.printInfo ("Can't remove currently used deployment " <> show (Dep.identifier tbRmDep) <> "!") (Config.interactive conf)
  where
    depId = Dep.identifier deployment
    tbRmDep' = Dep.getDeployment depId conf

rmComponent :: Int -> String -> Maybe FilePath -> IO ()
rmComponent ident component path =
  case path of
    Just p ->
      catch
        (removePathForcibly p)
        ( \e ->
            let err = show (e :: IOException)
             in Util.printInfo ("Couldn't remove " <> show ident <> " " <> component <> "; " <> err) False
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
            Dep.getDeployment x conf
          rmDep tbRmDep conf
          rmDeps keepDeps xs conf
  | otherwise = return ()

gcBroken :: [Int] -> Config.Config -> IO ()
gcBroken depIds conf =
  case depIds of
    [] -> return ()
    x : xs -> do
      checkDep x conf
      gcBroken xs conf

checkDep :: Int -> Config.Config -> IO ()
checkDep depId conf = do
  dep <- Dep.getDeployment depId conf
  let bootComps = Dep.bootComponents dep
  rootDirExists <- componentPresent $ Dep.rootDir dep
  bootDirExists <- componentPresent $ Dep.bootDir bootComps
  bootEntryExists <- componentPresent $ Dep.bootEntry bootComps
  ukiExists <- componentPresent $ Dep.ukiPath bootComps
  lockfileExists <- componentPresent $ Dep.lockfile dep
  idFileExists <- Util.pathExists $ Config.haldPath conf <> "/" <> show depId <> "/usr/.ald_dep"
  if rootDirExists
    && (bootDirExists || ukiExists)
    && (bootEntryExists || ukiExists)
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
          ( \e ->
              let err = show (e :: IOException)
               in Util.printInfo
                    ("Couldn't fix incorrect deployment id " <> show savedId <> "; " <> err)
                    (Config.interactive conf)
          )
    else
      if Dep.rootDir dep == Just "/usr"
        then return ()
        else
          Util.printInfo
            ("Collecting broken deployment " <> show depId <> "...")
            (Config.interactive conf)
            >> rmDep dep conf

componentPresent :: Maybe FilePath -> IO Bool
componentPresent compPath =
  case compPath of
    Just _ -> return True
    Nothing -> return False
