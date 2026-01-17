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

printDiff :: Int -> Int -> Config.Config -> IO ()
printDiff from to conf =
  if Config.packageManager conf == Config.Unknown
    then putStrLn "Unknown package manager, try setting packageManager in hald.conf."
    else do
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
      case compDeps of
        (x, y) -> do
          xStatus <- getStatus x conf
          yStatus <- getStatus y conf
          putStrLn $ "Comparing deployments " <> show x <> " and " <> show y
          diffStati xStatus yStatus

getStatus :: Int -> Config.Config -> IO String
getStatus dep conf =
  Dep.getDeployment dep conf >>= \fullDep ->
    let root =
          if Dep.rootDir fullDep == Just "/usr"
            then "/"
            else Data.Maybe.fromMaybe "" (Dep.rootDir fullDep)
     in (return $ listCmd (Config.packageManager conf) root)

listCmd :: Config.PackageManager -> String -> String
listCmd mgr root =
  case mgr of
    Config.Rpm -> "rpm -qa --root=" <> root
    Config.Unknown -> " "

diffStati :: String -> String -> IO ()
diffStati from to =
  catch
    ( callCommand
        ( Util.removeString "\n" from
            <> " | sort > /tmp/hald_from && "
            <> Util.removeString "\n" to
            <> " | sort > /tmp/hald_to"
        )
    )
    ( \e ->
        let _ = show (e :: IOException)
         in putStrLn "Failed to create temporary files. Is /tmp writable?"
    )
    >> catch
      (callCommand "diff -y /tmp/hald_from /tmp/hald_to | grep \"|\\|>\\|<\"")
      (\e -> let _ = show (e :: IOException) in putStrLn "No difference found.")
