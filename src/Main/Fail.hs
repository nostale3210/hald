module Main.Fail where

import Main.Container qualified as Container
import Main.Deployment qualified as Dep

failAndCleanup :: String -> Dep.Deployment -> IO ()
failAndCleanup err dep = do
  putStrLn "Fatal error!\nAttempting cleanup..."
  putStr "Failed deployment: "
  print dep
  Container.umountContainer "ald-root"
  Container.rmContainer "ald-root"
  error err
