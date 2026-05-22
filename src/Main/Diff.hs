{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main.Diff where

import Control.Exception (IOException, catch)
import Data.List (sortBy)
import Data.Maybe qualified
import Data.Ord (Down (..), comparing)
import Main.Config qualified as Config
import Main.Deployment qualified as Dep
import Main.Util qualified as Util
import System.IO (hClose, openTempFile)
import System.Process (readProcess)
import UnliftIO.Async (concurrently)

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
          (xStatus, yStatus) <- concurrently (getStatus x conf) (getStatus y conf)
          putStrLn $ "Comparing deployments " <> show x <> " and " <> show y
          diffStati xStatus yStatus

getStatus :: Int -> Config.Config -> IO String
getStatus dep conf =
  Dep.getDeployment dep conf >>= \fullDep ->
    let root = Data.Maybe.fromMaybe "" (Dep.rootDir fullDep)
     in (return $ listCmd (Config.packageManager conf) root)

listCmd :: Config.PackageManager -> String -> String
listCmd mgr root =
  case mgr of
    Config.Apk -> "apk list -I --root=" <> root <> " | sed \"s/\\s.*//\""
    Config.Pacman -> "pacman -Q --root=" <> root
    Config.Rpm -> "rpm -qa --root=" <> root <> " 2>/dev/null"
    Config.Xbps -> "xbps-query -l --rootdir=" <> root <> " | awk '{print $2}'"

diffStati :: String -> String -> IO ()
diffStati from to = do
  (tmpFrom, hFrom) <- openTempFile "/tmp" "hald_from"
  (tmpTo, hTo) <- openTempFile "/tmp" "hald_to"
  hClose hFrom
  hClose hTo
  catch
    ( do
        fromOutput <- readProcess "sh" ["-c", Util.removeString "\n" from <> " | sort"] ""
        writeFile tmpFrom fromOutput
        toOutput <- readProcess "sh" ["-c", Util.removeString "\n" to <> " | sort"] ""
        writeFile tmpTo toOutput
    )
    ( \e ->
        let _ = show (e :: IOException)
         in putStrLn "Failed to create temporary files. Is /tmp writable?"
    )
    >> catch
      ( do
          diffOutput <- readProcess "sh" ["-c", "diff -y " <> tmpFrom <> " " <> tmpTo <> " | grep \"|\\|>\\|<\""] ""
          putStr diffOutput
      )
      (\e -> let _ = show (e :: IOException) in putStrLn "No difference found.")
