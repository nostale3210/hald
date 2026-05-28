module Hald.Diff where

import Control.Exception (IOException, catch)
import Data.Char (isSpace)
import Data.List (find, sort, sortBy)
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..), comparing)
import Hald.Config qualified as Config
import Hald.Deployment qualified as Dep
import Hald.Util qualified as Util
import System.Exit (ExitCode (..))
import System.IO (hClose, openTempFile)
import System.Process (readProcessWithExitCode)
import UnliftIO.Async (concurrently)

selectDeployment :: [Int] -> [Int] -> Int
selectDeployment allDeps selected =
  fromMaybe 0 $ find (`notElem` selected) $ sortBy (comparing Down) allDeps

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
getStatus dep conf = do
  fullDep <- Dep.getDeployment dep conf
  let root = fromMaybe "" (Dep.rootDir fullDep)
      PkgQuery cmd args post = listCmd (Config.packageManager conf) root
  Util.ioOrDefault "" $ post <$> Util.quietReadProcess cmd args ""

data PkgQuery = PkgQuery
  { pqCmd :: FilePath,
    pqArgs :: [String],
    pqPost :: String -> String
  }

listCmd :: Config.PackageManager -> String -> PkgQuery
listCmd mgr root =
  case mgr of
    Config.Apk ->
      PkgQuery
        "apk"
        ["list", "-I", "--root=" <> root]
        (unlines . map (takeWhile (not . isSpace)) . lines)
    Config.Pacman ->
      PkgQuery
        "pacman"
        ["-Q", "--root=" <> root]
        id
    Config.Rpm ->
      PkgQuery
        "rpm"
        ["-qa", "--root=" <> root]
        id
    Config.Xbps ->
      PkgQuery
        "xbps-query"
        ["-l", "--rootdir=" <> root]
        (unlines . map (unwords . drop 1 . words) . lines)
    Config.Unknown -> PkgQuery "" [] id

diffStati :: String -> String -> IO ()
diffStati from to = do
  (tmpFrom, hFrom) <- openTempFile "/tmp" "hald_from"
  (tmpTo, hTo) <- openTempFile "/tmp" "hald_to"
  hClose hFrom
  hClose hTo
  catch
    ( do
        writeFile tmpFrom (unlines . sort . lines $ from)
        writeFile tmpTo (unlines . sort . lines $ to)
    )
    ( \e ->
        let _ = show (e :: IOException)
         in putStrLn "Failed to create temporary files. Is /tmp writable?"
    )
  (ec, diffOutput, _) <- readProcessWithExitCode "diff" ["-y", tmpFrom, tmpTo] ""
  case ec of
    ExitSuccess -> putStrLn "No difference found."
    _ -> putStr . unlines . filter (any (`elem` "|><")) . lines $ diffOutput
