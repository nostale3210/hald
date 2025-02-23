module Main.Deployment where

import Control.Exception (IOException, catch)
import Data.Set qualified as Set
import Main.Util qualified as Util
import System.FilePath.Glob
import System.Posix.Files

data Deployment
  = Deployment
  { identifier :: Int,
    lockfile :: Maybe FilePath,
    rootDir :: Maybe FilePath,
    bootComponents :: BootComponents
  }
  deriving (Show, Eq)

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
          lockfile = Just (haldPath <> "/." <> show depId),
          rootDir = Just (haldPath <> "/" <> show depId),
          bootComponents = createBootPaths depId
        }

createBootPaths :: Int -> BootComponents
createBootPaths depId =
  BootComponents
    { bootDir = Just (bootPath <> "/" <> show depId),
      bootEntry = Just (bootPath <> "/loader/entries/" <> show depId <> ".conf")
    }

getBootComponents :: Int -> IO BootComponents
getBootComponents depId = do
  let bPath = bootPath <> "/" <> show depId
      bEntry = bootPath <> "/loader/entries/" <> show depId <> ".conf"
  bPathExists <-
    catch
      (fileExist bPath)
      ( \e -> do
          let _ = show (e :: IOException)
          return False
      )
  bPathIsDir <-
    if bPathExists
      then do
        bPathStatus <- getFileStatus bPath
        return $ isDirectory bPathStatus
      else
        return False
  kernelExists <-
    catch
      (fileExist (bPath <> "/vmlinuz"))
      ( \e -> do
          let _ = show (e :: IOException)
          return False
      )
  initrdExists <-
    catch
      (fileExist (bPath <> "/initramfs.img"))
      ( \e -> do
          let _ = show (e :: IOException)
          return False
      )
  bEntryExists <-
    catch
      (fileExist bEntry)
      ( \e -> do
          let _ = show (e :: IOException)
          return False
      )
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
  let lFile = haldPath <> "/." <> show depId
      rDir = haldPath <> "/" <> show depId
  lFileExists <-
    catch
      (fileExist lFile)
      ( \e -> do
          let _ = show (e :: IOException)
          return False
      )
  rDirExists <-
    catch
      (fileExist rDir)
      ( \e -> do
          let _ = show (e :: IOException)
          return False
      )
  rDirIsDir <-
    if rDirExists
      then do
        rDirStatus <- getFileStatus rDir
        let rDirIsDir = isDirectory rDirStatus
        return rDirIsDir
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
  lockfiles <- globDir1 (compile ".[0-9]*") haldPath
  rootDirs <- globDir1 (compile "[0-9]*") haldPath
  bootDirs <- globDir1 (compile "[0-9]*") bootPath
  bootEntrys <- globDir1 (compile "[0-9]*.conf") (bootPath <> "/loader/entries")
  let lockSet = Set.fromList $ map (Util.removeString "." . Util.removeString (haldPath <> "/")) lockfiles
      rootSet = Set.fromList $ map (Util.removeString (haldPath <> "/")) rootDirs
      bootDSet = Set.fromList $ map (Util.removeString (bootPath <> "/")) bootDirs
      bootESet = Set.fromList $ map (Util.removeString ".conf" . Util.removeString (bootPath <> "/loader/entries/")) bootEntrys
  return $ Set.toList $ Set.union bootESet . Set.union bootDSet . Set.union lockSet $ rootSet

getDeploymentsInt :: IO [Int]
getDeploymentsInt = do
  deployments <- getDeployments
  let intdeps = map (\d -> read d :: Int) deployments
  return intdeps

haldPath :: FilePath
haldPath = "/.ald"

bootPath :: FilePath
bootPath = "/boot"

deployment :: String
deployment = "Deployment"
