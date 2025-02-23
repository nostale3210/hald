module Main where

import Main.Deployment (getDeployment, getDeploymentsInt)
import Main.Space (gcBroken)

main :: IO ()
main = do
    depls <- getDeploymentsInt
    gcBroken depls

showDeps :: [Int] -> IO ()
showDeps depl =
  case depl of
    [] -> putStrLn "No deployment left."
    x : xs -> do
      dep <- getDeployment x
      print dep
      showDeps xs
