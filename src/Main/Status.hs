module Main.Status where

import Control.Exception (IOException, catch)
import Data.List (isPrefixOf, sort)
import Main.Config qualified as Config
import Main.Deployment qualified as Dep
import Main.Util qualified as Util
import System.FilePath (takeBaseName, takeDirectory)
import System.FilePath.Glob (compile, globDir1)

data DepStatus
  = DepStatus
  { identifier :: Int,
    name :: String,
    version :: String,
    kernel :: String
  }
  deriving (Show)

getDepStatus :: Config.Config -> Dep.Deployment -> IO DepStatus
getDepStatus conf dep = do
  currentDepId <- Dep.getCurrentDeploymentId $ Config.rootDir conf
  let osReleasePath =
        if currentDepId == Dep.identifier dep
          then Config.rootDir conf <> "/usr/lib/os-release"
          else Config.haldPath conf <> "/" <> show (Dep.identifier dep) <> "/usr/lib/os-release"
  osReleaseExists <- Util.pathExists osReleasePath
  osReleaseContent <-
    if osReleaseExists
      then
        catch
          (readFile osReleasePath)
          ( \e ->
              let err = show (e :: IOException)
               in return $ "Couldn't read os-release; " <> err
          )
      else return "No os-release file found."
  let osReleaseLines = lines osReleaseContent
  kernelVer <- returnKernelVersion osReleasePath
  return
    DepStatus
      { identifier = Dep.identifier dep,
        name = returnOsField "NAME" osReleaseLines,
        version = returnOsField "VERSION" osReleaseLines,
        kernel = kernelVer
      }

returnOsField :: String -> [String] -> String
returnOsField field content =
  case content of
    [] -> ""
    x : xs ->
      if field `isPrefixOf` x
        then unwords . tail . words $ Util.replaceString "\"" "" $ Util.replaceString "=" " " x
        else returnOsField field xs

returnKernelVersion :: FilePath -> IO String
returnKernelVersion fp =
  globDir1 (compile "*") (takeDirectory fp <> "/modules") >>= \modulesContent ->
    return . takeBaseName $ unwords modulesContent

printDepStati :: Config.Config -> IO ()
printDepStati conf = do
  allDeps <- Dep.getDeploymentsInt (Config.haldPath conf) (Config.bootPath conf)
  let sortedDeps = sort allDeps
  depList <-
    mapM
      ( \dep ->
          Dep.getDeployment dep conf >>= \fullDep ->
            getDepStatus
              conf
              fullDep
      )
      sortedDeps
  putStrLn "Currently retained deployments:"
  printDep (reverse depList) conf

printDep :: [DepStatus] -> Config.Config -> IO ()
printDep deps conf =
  case deps of
    [] -> return ()
    x : xs -> do
      currentDepId <- Dep.getCurrentDeploymentId $ Config.rootDir conf
      let status =
            if currentDepId == identifier x
              then "\t(Active)"
              else ""
      putStrLn
        ( "\n"
            <> twospaces "Deployment "
            <> show (identifier x)
            <> status
            <> "\n"
            <> fourspaces (name x)
            <> "\n"
            <> fourspaces ("Version: " <> version x)
            <> "\n"
            <> fourspaces ("Kernel: " <> kernel x)
        )
      printDep xs conf
      where
        twospaces y = "  " <> y
        fourspaces y = "    " <> y
