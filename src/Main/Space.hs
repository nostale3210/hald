module Main.Space where

import Control.Exception (IOException)
import Control.Exception.Base (catch)
import Main.Deployment qualified as Dep
import System.Directory

rmDep :: Dep.Deployment -> IO ()
rmDep deployment = do
  let depId = Dep.identifier deployment
  tbRmDep <- Dep.getDeployment depId
  let tbRmBootComponents = Dep.bootComponents tbRmDep
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
    Nothing -> putStrLn ("No associated " <> component <> ".")

rmDeps :: Int -> [Int] -> IO ()
rmDeps keepDeps deployments
  | keepDeps < length deployments =
      case deployments of
        [] -> putStrLn "No deployments remaining."
        x : xs -> do
          tbRmDep <- Dep.getDeployment x
          rmDep tbRmDep
          rmDeps keepDeps xs
  | otherwise = putStrLn "No deployments to remove."

gcBroken :: [Int] -> IO ()
gcBroken depIds = do
  case depIds of
    [] -> putStrLn "No remaining defective deployments."
    x : xs -> do
      checkDep x
      gcBroken xs
  where
    checkDep depId = do
      dep <- Dep.getDeployment depId
      let bootComps = Dep.bootComponents dep
      case Dep.rootDir dep of
        Just _ -> do
          putStrLn ("Found root dir for deployment " <> show depId <> ".")
          case Dep.bootDir bootComps of
            Just _ -> do
              putStrLn ("Found boot dir for deployment " <> show depId <> ".")
              case Dep.bootEntry bootComps of
                Just _ -> do
                  putStrLn ("Found boot entry for deployment " <> show depId <> ".")
                  case Dep.lockfile dep of
                    Just _ -> putStrLn ("Found lockfile for deployment " <> show depId <> ".")
                    Nothing -> rmDep dep
                Nothing -> rmDep dep
            Nothing -> rmDep dep
        Nothing -> rmDep dep
