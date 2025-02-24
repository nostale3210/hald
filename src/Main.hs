module Main where

import Main.Create (createNewDeployment)
import Main.Deployment (createDeployment, getDeployment, getDeploymentsInt)

main :: IO ()
main = do
  deployments <- getDeploymentsInt
  let newDeployment = createDeployment deployments
  createNewDeployment newDeployment

showDeps :: [Int] -> IO ()
showDeps depl =
  case depl of
    [] -> return ()
    x : xs -> do
      dep <- getDeployment x
      print dep
      showDeps xs
