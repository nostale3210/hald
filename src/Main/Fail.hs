module Main.Fail where

import Main.Container qualified as Container
import Main.Deployment qualified as Dep
import Main.Space qualified as Space

failAndCleanup :: String -> Dep.Deployment -> IO ()
failAndCleanup err dep = do
  putStrLn "Fatal error!\nAttempting cleanup..."
  currentDeps <- Dep.getDeploymentsInt
  Space.rmDep dep
  Space.gcBroken currentDeps
  Container.umountContainer "ald-root"
  Container.rmContainer "ald-root"
  error err
