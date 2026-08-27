module Hald.Deployment where

import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Set qualified as Set
import Hald.Config qualified as Config
import Hald.Util qualified as Util
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))
import Text.Read (readMaybe)

data Backend = Hardlink | Cas deriving (Show, Eq)

data Deployment
  = Deployment
  { identifier :: Int,
    backend :: Backend,
    lockfile :: Maybe FilePath,
    rootDir :: Maybe FilePath,
    bootComponents :: BootComponents
  }
  deriving (Show, Eq)

data BootComponents
  = BootComponents
  { bootDir :: Maybe FilePath,
    bootEntry :: Maybe FilePath,
    ukiPath :: Maybe FilePath
  }
  deriving (Show, Eq)

rootDirFor :: Config.Config -> Int -> FilePath
rootDirFor conf depId = Config.haldPath conf </> "trees" </> show depId

lockfileFor :: Config.Config -> Int -> FilePath
lockfileFor conf depId = Config.haldPath conf </> "trees/." <> show depId

createDeployment :: [Int] -> Config.Config -> Backend -> Deployment
createDeployment exDeps conf backend =
  let depId = Util.newIdentifier exDeps
   in Deployment
        { identifier = depId,
          backend = backend,
          lockfile = Just (lockfileFor conf depId),
          rootDir = Just (rootDirFor conf depId),
          bootComponents = createBootPaths depId conf
        }

dummyDeployment :: Deployment
dummyDeployment =
  Deployment
    { identifier = -1,
      backend = Cas,
      lockfile = Nothing,
      rootDir = Nothing,
      bootComponents =
        BootComponents
          { bootDir = Nothing,
            bootEntry = Nothing,
            ukiPath = Nothing
          }
    }

createBootPaths :: Int -> Config.Config -> BootComponents
createBootPaths depId conf =
  BootComponents
    { bootDir = Just (Config.bootPath conf </> show depId),
      bootEntry = Just (Config.bootPath conf <> "/loader/entries/" <> show depId <> ".conf"),
      ukiPath = Just (Config.ukiPath conf </> show depId <> ".efi")
    }

getBootComponents :: Int -> Config.Config -> IO BootComponents
getBootComponents depId conf = do
  let bPath = Config.bootPath conf </> show depId
      bEntry = Config.bootPath conf <> "/loader/entries/" <> show depId <> ".conf"
      uki = Config.ukiPath conf </> show depId <> ".efi"
  bPathExists <- Util.pathExists bPath
  bPathIsDir <-
    if bPathExists
      then
        doesDirectoryExist bPath
      else
        return False
  kernelExists <- Util.pathExists (bPath <> "/vmlinuz")
  initrdExists <- Util.pathExists (bPath <> "/initramfs.img")
  bEntryExists <- Util.pathExists bEntry
  ukiExists <- Util.pathExists uki
  return
    BootComponents
      { bootDir =
          if bPathExists
            && bPathIsDir
            && kernelExists
            && initrdExists
            then Just bPath
            else Nothing,
        bootEntry =
          if bEntryExists
            then Just bEntry
            else Nothing,
        ukiPath =
          if ukiExists
            then Just uki
            else Nothing
      }

getDeployment :: Int -> Config.Config -> IO Deployment
getDeployment depId conf = do
  let rDir = rootDirFor conf depId
      markerFile = rDir </> "backend"
  rDirExists <- Util.pathExists rDir
  rDirIsDir <-
    if rDirExists
      then doesDirectoryExist rDir
      else return False
  markerExists <- Util.pathExists markerFile
  backend <-
    if markerExists
      then
        readFile markerFile >>= \content ->
          case listToMaybe (lines content) of
            Just "Cas" -> return Cas
            _ -> return Hardlink
      else return Hardlink
  bComponents <- getBootComponents depId conf
  return
    Deployment
      { identifier = depId,
        backend = backend,
        lockfile = Just (lockfileFor conf depId),
        rootDir = if rDirExists && rDirIsDir then Just rDir else Nothing,
        bootComponents = bComponents
      }

getDeploymentsInt :: Config.Config -> IO [Int]
getDeploymentsInt conf = do
  let bp = Config.bootPath conf
      ep = bp <> "/loader/entries"
      up = Config.ukiPath conf
  treeDeps <- findDeploymentIds conf
  bdEntries <- Util.listDirSafe bp
  beEntries <- Util.listDirSafe ep
  ukiEntries <- Util.listDirSafe up
  let bootIds =
        mapMaybe readMaybe bdEntries
          <> mapMaybe (stripExt ".conf") beEntries
          <> mapMaybe (stripExt ".efi") ukiEntries
  return $ Set.toList $ Set.fromList (treeDeps <> bootIds)
  where
    stripExt ext = readMaybe . Util.removeString ext

getCurrentDeploymentId :: FilePath -> IO Int
getCurrentDeploymentId root = Data.Maybe.fromMaybe 0 <$> readDepLockfile root

findDeploymentIds :: Config.Config -> IO [Int]
findDeploymentIds conf =
  mapMaybe treeEntryId <$> Util.listDirSafe (Config.haldPath conf </> "trees")
  where
    treeEntryId = readMaybe . Util.removeString "."

readDepLockfile :: FilePath -> IO (Maybe Int)
readDepLockfile root = do
  markerExists <- Util.pathExists markerPath
  if markerExists
    then Just . parseDepId <$> readFile markerPath
    else return Nothing
  where
    markerPath = root </> "usr/.hald_dep"
    parseDepId content =
      case lines content of
        (l : _) -> maybe 0 fst $ listToMaybe $ reads l
        [] -> 0
