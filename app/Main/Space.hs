module Main.Space where

import Main.Deployment qualified as Dep
import System.Directory

rmDep :: IO Dep.Deployment -> IO ()
rmDep deployment = do
  deployment <- deployment
  let depId = Dep.identifier deployment
  tbRmDep <- Dep.getDeployment depId
  let tbRmBootComponents = Dep.bootComponents tbRmDep
  case Dep.rootDir tbRmDep of
    Just x -> removePathForcibly x
    Nothing -> putStrLn "No associated root directory."
  case Dep.bootDir tbRmBootComponents of
    Just x -> removePathForcibly x
    Nothing -> putStrLn "No associated boot directory."
  case Dep.bootEntry tbRmBootComponents of
    Just x -> removePathForcibly x
    Nothing -> putStrLn "No associated boot entry."
  case Dep.lockfile tbRmDep of
    Just x -> removePathForcibly x
    Nothing -> putStrLn "No associated lock file."
