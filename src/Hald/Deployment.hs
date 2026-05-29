module Hald.Deployment where

import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Set qualified as Set
import Hald.Config qualified as Config
import Hald.Legacy qualified as Legacy
import Hald.Util qualified as Util
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))

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

createDeployment :: [Int] -> Config.Config -> Backend -> Deployment
createDeployment exDeps conf backend =
  let depId = Util.newIdentifier exDeps
   in Deployment
        { identifier = depId,
          backend = backend,
          lockfile = Just (Legacy.treeLockfile conf depId),
          rootDir = Just (Legacy.treeRootDir conf depId),
          bootComponents = createBootPaths depId conf
        }

dummyDeployment :: Deployment
dummyDeployment =
  Deployment
    { identifier = -1,
      backend = Hardlink,
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
  lFile <- Legacy.resolveLockfile conf depId
  rDir <- Legacy.resolveRootDir conf depId
  let markerFile = case rDir of
        Just d -> d <> "/backend"
        Nothing -> Legacy.treeRootDir conf depId <> "/backend"
  rDirExists <- case rDir of
    Just d -> Util.pathExists d
    Nothing -> return False
  rDirIsDir <-
    if rDirExists
      then case rDir of
        Just d -> doesDirectoryExist d
        Nothing -> return False
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
        lockfile = lFile,
        rootDir = if rDirExists && rDirIsDir then rDir else Nothing,
        bootComponents = bComponents
      }

isNumericConf :: String -> Bool
isNumericConf f =
  not (null f)
    && head f `elem` ['0' .. '9']
    && let l = length f in l >= 6 && drop (l - 5) f == ".conf"

getDeployments :: Config.Config -> IO [FilePath]
getDeployments conf = do
  let bp = Config.bootPath conf
      ep = bp <> "/loader/entries"
      up = Config.ukiPath conf
  depIds <- Legacy.findDeploymentIds conf
  bdEntries <- Util.listDirSafe bp
  beEntries <- Util.listDirSafe ep
  ukiEntries <- Util.listDirSafe up
  let bootDirs = map (bp </>) $ filter Util.startsWithDigit bdEntries
      bootEntrys = map (ep </>) $ filter isNumericConf beEntries
      ukis = map (up </>) $ filter Util.startsWithDotDigit ukiEntries
      bootDSet = Set.fromList $ map (Util.removeString (bp <> "/")) bootDirs
      bootESet = Set.fromList $ map (Util.removeString ".conf" . Util.removeString (ep <> "/")) bootEntrys
      ukiSet = Set.fromList $ map (Util.removeString ".efi" . Util.removeString (up <> "/")) ukis
  return $ Set.toList $ Set.union ukiSet $ Set.union bootESet . Set.union bootDSet $ Set.fromList (map show depIds)

getDeploymentsInt :: Config.Config -> IO [Int]
getDeploymentsInt = fmap (mapMaybe parseId) . getDeployments
  where
    parseId s = fmap fst (listToMaybe (reads s))

getCurrentDeploymentId :: FilePath -> IO Int
getCurrentDeploymentId root = Data.Maybe.fromMaybe 0 <$> Legacy.readDepLockfile root
