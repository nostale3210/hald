module Hald.Space where

import Control.Exception (IOException, catch)
import Control.Monad (when)
import Data.List (sort)
import Data.Maybe (isJust)
import Hald.Config qualified as Config
import Hald.Deployment qualified as Dep
import Hald.Lock qualified as Lock
import Hald.Util qualified as Util
import System.Directory (removePathForcibly)
import System.FilePath ((</>))
import UnliftIO (pooledMapConcurrently_)
import UnliftIO.Async (pooledForConcurrently_)

rmDep :: Dep.Deployment -> Config.Config -> IO ()
rmDep deployment conf = do
  tbRmDep <- tbRmDep'
  currentDepId <- Dep.getCurrentDeploymentId (Config.rootDir conf)
  let bootComponents = Dep.bootComponents tbRmDep
  bootPathNoComps <- Util.pathExists (Config.bootPath conf </> show depId)
  let tbRmBootComponents =
        if bootPathNoComps
          then
            Dep.BootComponents
              { Dep.bootDir = Just (Config.bootPath conf </> show depId),
                Dep.bootEntry = Dep.bootEntry bootComponents,
                Dep.ukiPath = Dep.ukiPath bootComponents
              }
          else
            bootComponents
  if Dep.identifier tbRmDep /= currentDepId
    then do
      Lock.clearRecursiveImmutable (Config.haldPath conf </> show depId)
      mapM_
        (uncurry (rmComponent depId conf))
        [ ("root dir", Dep.rootDir tbRmDep),
          ("boot dir", Dep.bootDir tbRmBootComponents),
          ("boot entry", Dep.bootEntry tbRmBootComponents),
          ("uki", Dep.ukiPath tbRmBootComponents),
          ("lockfile", Dep.lockfile tbRmDep)
        ]
    else
      Util.printInfo ("Can't remove currently used deployment " <> show (Dep.identifier tbRmDep) <> "!") (Config.interactive conf)
  where
    depId = Dep.identifier deployment
    tbRmDep' = Dep.getDeployment depId conf

rmComponent :: Int -> Config.Config -> String -> Maybe FilePath -> IO ()
rmComponent ident conf component path =
  case path of
    Just p ->
      catch
        (removePathForcibly p)
        ( \e ->
            let err = show (e :: IOException)
             in Util.printInfo ("Couldn't remove " <> show ident <> " " <> component <> "; " <> err) (Config.interactive conf)
        )
    Nothing ->
      Util.printInfo
        ("Deployment " <> show ident <> ": No associated " <> component <> ".")
        (Config.interactive conf)

rmDeps :: Int -> [Int] -> Config.Config -> IO ()
rmDeps keepDeps deployments conf = do
  let sorted = sort deployments
      numToRemove = length sorted - keepDeps
  when (numToRemove > 0) $
    pooledForConcurrently_ (take numToRemove sorted) $ \depId -> do
      tbRmDep <- Dep.getDeployment depId conf
      rmDep tbRmDep conf

gcBroken :: [Int] -> Config.Config -> IO ()
gcBroken depIds conf = pooledMapConcurrently_ (`checkDep` conf) depIds

checkDep :: Int -> Config.Config -> IO ()
checkDep depId conf = do
  dep <- Dep.getDeployment depId conf
  let bootComps = Dep.bootComponents dep
  rootDirExists <- componentPresent $ Dep.rootDir dep
  bootDirExists <- componentPresent $ Dep.bootDir bootComps
  bootEntryExists <- componentPresent $ Dep.bootEntry bootComps
  ukiExists <- componentPresent $ Dep.ukiPath bootComps
  lockfileExists <- componentPresent $ Dep.lockfile dep
  idFileExists <- Util.pathExists $ Config.haldPath conf </> show depId <> "/usr/.ald_dep"
  if rootDirExists
    && (bootDirExists || ukiExists)
    && (bootEntryExists || ukiExists)
    && lockfileExists
    && idFileExists
    then do
      idFileContent <- readFile $ Config.haldPath conf </> show depId <> "/usr/.ald_dep"
      let savedId = case reads (head $ lines idFileContent) of [(n, "")] -> n; _ -> 0
      when
        (savedId /= depId)
        $ catch
          ( writeFile
              (Config.haldPath conf </> show depId <> "/usr/.ald_dep")
              (show depId)
          )
          ( \e ->
              let err = show (e :: IOException)
               in Util.printInfo
                    ("Couldn't fix incorrect deployment id " <> show savedId <> "; " <> err)
                    (Config.interactive conf)
          )
    else
      Util.printInfo
        ("Collecting broken deployment " <> show depId <> "...")
        (Config.interactive conf)
        >> rmDep dep conf

componentPresent :: Maybe FilePath -> IO Bool
componentPresent = return . isJust
