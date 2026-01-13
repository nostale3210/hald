module Main.Diff where

import Control.Exception (IOException, catch)
import Data.List (sortBy)
import Data.Maybe qualified
import Data.Ord (Down (..), comparing)
import Main.Config qualified as Config
import Main.Deployment qualified as Dep
import Main.Util qualified as Util
import System.Process (callCommand)

selectDeployment :: [Int] -> [Int] -> Int
selectDeployment allDeps selected =
  case sortBy (comparing Data.Ord.Down) allDeps of
    [] -> 0
    x : xs ->
      if x `elem` selected
        then selectDeployment xs selected
        else x

selectDeps :: Int -> Int -> Config.Config -> IO (Int, Int)
selectDeps comp compTo conf
  | comp == 0 && compTo == 0 = do
      compDeps <- Dep.getDeploymentsInt conf
      toDep <- Dep.getCurrentDeploymentId $ Config.rootDir conf
      let fromDep = selectDeployment compDeps [toDep]
      return (if fromDep < toDep then (fromDep, toDep) else (toDep, fromDep))
  | comp /= 0 && compTo == 0 = do
      toDep <- Dep.getCurrentDeploymentId $ Config.rootDir conf
      if comp /= toDep then return (comp, toDep) else selectDeps 0 0 conf
  | comp == 0 && compTo /= 0 = selectDeps compTo comp conf
  | otherwise = return (comp, compTo)

printDiff :: Int -> Int -> String -> Config.Config -> IO ()
printDiff from to cmd conf = do
  allDeps <- Dep.getDeploymentsInt conf
  let fromExisting =
        if from `elem` allDeps || from == 0
          then from
          else error "Deployment 1 doesn't exist."
      toExisting =
        if to `elem` allDeps || to == 0
          then to
          else error "Deployment 2 doesn't exist."
  compDeps <- selectDeps fromExisting toExisting conf
  case (compDeps, cmd) of
    ((x, y), "rpm") -> do
      xStatus <- getRpmStatus x conf
      yStatus <- getRpmStatus y conf
      putStrLn $ "Comparing deployments " <> show x <> " and " <> show y
      diffStati xStatus yStatus
    _ -> return ()

getRpmStatus :: Int -> Config.Config -> IO String
getRpmStatus dep conf =
  Dep.getDeployment dep conf >>= \fullDep ->
    let root =
          if Dep.rootDir fullDep == Just "/usr"
            then "/"
            else Data.Maybe.fromMaybe "" (Dep.rootDir fullDep)
     in (return $ "rpm -qa --root=" <> root)

diffStati :: String -> String -> IO ()
diffStati from to =
  catch
    ( callCommand
        ( "diff -y <("
            <> Util.removeString "\n" from
            <> " | sort) <("
            <> Util.removeString "\n" to
            <> " | sort) | grep \"|\\|>\\|<\""
        )
    )
    (\e -> let _ = show (e :: IOException) in putStrLn "No difference found.")
