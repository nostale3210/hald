module Hald.Status where

import Control.Exception (IOException, catch)
import Control.Monad (forM_)
import Data.List (find, isPrefixOf, sort)
import Data.Maybe (fromMaybe)
import Hald.Config qualified as Config
import Hald.Deployment qualified as Dep
import Hald.Util qualified as Util
import System.FilePath (takeBaseName, takeDirectory)
import UnliftIO.Async (forConcurrently)

data DepStatus
  = DepStatus
  { identifier :: Int,
    name :: String,
    version :: String,
    kernel :: String,
    backend :: String
  }
  deriving (Show)

getDepStatus :: Config.Config -> Dep.Deployment -> IO DepStatus
getDepStatus conf dep = do
  currentDepId <- Dep.getCurrentDeploymentId $ Config.rootDir conf
  let depRoot = fromMaybe "" (Dep.rootDir dep)
      osReleasePath =
        if currentDepId == Dep.identifier dep
          then Config.rootDir conf <> "/usr/lib/os-release"
          else depRoot <> "/usr/lib/os-release"
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
        kernel = kernelVer,
        backend = show $ Dep.backend dep
      }

returnOsField :: String -> [String] -> String
returnOsField field content =
  maybe
    ""
    (unwords . tail . words . Util.replaceString "\"" "" . Util.replaceString "=" " ")
    $ find (field `isPrefixOf`) content

returnKernelVersion :: FilePath -> IO String
returnKernelVersion fp =
  takeBaseName . unwords <$> Util.listDirSafe (takeDirectory fp <> "/modules")

printDepStati :: Config.Config -> Bool -> IO ()
printDepStati conf comp = do
  allDeps <- Dep.getDeploymentsInt conf
  let sortedDeps = sort allDeps
  depList <-
    forConcurrently sortedDeps $ \dep ->
      Dep.getDeployment dep conf >>= \fullDep ->
        getDepStatus
          conf
          fullDep
  putStrLn "Currently retained deployments:"
  printDep depList comp conf

printDep :: [DepStatus] -> Bool -> Config.Config -> IO ()
printDep deps comp conf =
  Dep.getCurrentDeploymentId (Config.rootDir conf) >>= \dId ->
    forM_ deps $ putStrLn . getDepString comp dId

getDepString :: Bool -> Int -> DepStatus -> String
getDepString comp dId ds =
  let status = (if dId == identifier ds then "\t(Active)" else "") <> "\n"
      lead y =
        if dId == identifier ds
          then " • " <> y
          else replicate 3 ' ' <> y
      sublead y = replicate 5 ' ' <> y
      depLine = lead "Deployment " <> show (identifier ds)
      fields = [("Version", version), ("Kernel", kernel), ("Backend", backend)]
      maxFieldLength = maximum $ map (length . fst) fields
      fieldLine (name, field) =
        "\n"
          <> sublead
            ( name
                <> ": "
                <> replicate (maxFieldLength - length name) ' '
                <> field ds
            )
      fieldLines = concatMap fieldLine fields
   in if comp
        then depLine <> " - " <> version ds
        else "\n" <> depLine <> status <> sublead (name ds) <> fieldLines
