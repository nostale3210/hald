module Main.Deployment where

import Control.Exception (IOException, catch)
import Data.Set qualified as Set
import Main.Config qualified as Config
import Main.Util qualified as Util
import System.Directory (doesDirectoryExist)
import System.FilePath.Glob

data Deployment
  = Deployment
  { identifier :: Int,
    lockfile :: Maybe FilePath,
    rootDir :: Maybe FilePath,
    bootComponents :: BootComponents
  }
  deriving (Show, Eq)

instance Ord Deployment where
  (<) depA depB = identifier depA < identifier depB
  (<=) depA depB = identifier depA <= identifier depB

data BootComponents
  = BootComponents
  { bootDir :: Maybe FilePath,
    bootEntry :: Maybe FilePath
  }
  deriving (Show, Eq)

createDeployment :: [Int] -> Deployment
createDeployment exDeps =
  let depId = Util.newIdentifier exDeps
   in Deployment
        { identifier = depId,
          lockfile = Just (Config.haldPath <> "/." <> show depId),
          rootDir = Just (Config.haldPath <> "/" <> show depId),
          bootComponents = createBootPaths depId
        }

createBootPaths :: Int -> BootComponents
createBootPaths depId =
  BootComponents
    { bootDir = Just (Config.bootPath <> "/" <> show depId),
      bootEntry = Just (Config.bootPath <> "/loader/entries/" <> show depId <> ".conf")
    }

getBootComponents :: Int -> IO BootComponents
getBootComponents depId = do
  let bPath = Config.bootPath <> "/" <> show depId
      bEntry = Config.bootPath <> "/loader/entries/" <> show depId <> ".conf"
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
            else Nothing
      }

getDeployment :: Int -> IO Deployment
getDeployment depId = do
  currentDepId <- getCurrentDeploymentId
  let lFile = Config.haldPath <> "/." <> show depId
      rDir = if depId == currentDepId then "/usr" else Config.haldPath <> "/" <> show depId
  lFileExists <- Util.pathExists lFile
  rDirExists <- Util.pathExists rDir
  rDirIsDir <-
    if rDirExists
      then do
        doesDirectoryExist rDir
      else return False
  bComponents <- getBootComponents depId
  return
    Deployment
      { identifier = depId,
        lockfile = if lFileExists then Just lFile else Nothing,
        rootDir = if rDirExists && rDirIsDir then Just rDir else Nothing,
        bootComponents = bComponents
      }

getDeployments :: IO [FilePath]
getDeployments = do
  lockfiles <- globDir1 (compile ".[0-9]*") Config.haldPath
  rootDirs <- globDir1 (compile "[0-9]*") Config.haldPath
  bootDirs <- globDir1 (compile "[0-9]*") Config.bootPath
  bootEntrys <- globDir1 (compile "[0-9]*.conf") (Config.bootPath <> "/loader/entries")
  let lockSet = Set.fromList $ map (Util.removeString "." . Util.removeString (Config.haldPath <> "/")) lockfiles
      rootSet = Set.fromList $ map (Util.removeString (Config.haldPath <> "/")) rootDirs
      bootDSet = Set.fromList $ map (Util.removeString (Config.bootPath <> "/")) bootDirs
      bootESet = Set.fromList $ map (Util.removeString ".conf" . Util.removeString (Config.bootPath <> "/loader/entries/")) bootEntrys
  return $ Set.toList $ Set.union bootESet . Set.union bootDSet . Set.union lockSet $ rootSet

getDeploymentsInt :: IO [Int]
getDeploymentsInt = do
  deployments <- getDeployments
  let intdeps = map (\d -> read d :: Int) deployments
  return intdeps

getCurrentDeploymentId :: IO Int
getCurrentDeploymentId = do
  content <-
    catch
      (readFile "/usr/.ald_dep")
      ( \e -> do
          let err = show (e :: IOException)
          putStrLn $ "Couldn't read deployment ID; " <> err
          return "0"
      )
  return (read (head $ lines content) :: Int)

getDeploymentId :: Int -> IO Int
getDeploymentId depId = do
  content <- readFile (Config.haldPath <> "/" <> show depId <> "/usr/.ald_dep")
  return (read (head $ lines content) :: Int)
