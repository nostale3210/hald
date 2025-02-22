module Main.Deployment where

import System.Posix.Files

data Deployment
  = Deployment
  { identifier :: Int,
    lockfile :: Maybe FilePath,
    rootDir :: Maybe FilePath,
    bootComponents :: BootComponents
  }

data BootComponents
  = BootComponents
  { bootDir :: Maybe FilePath,
    bootEntry :: Maybe FilePath
  }

newIdentifier :: [Int] -> Int
newIdentifier = succ . maximum

createDeployment :: [Int] -> Deployment
createDeployment exDeps =
  let depId = newIdentifier exDeps
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
  bPathExists <- fileExist bPath
  bPathIsDir <-
    if bPathExists
      then do
        bPathStatus <- getFileStatus bPath
        let bPathIsDir = isDirectory bPathStatus
        return bPathIsDir
      else return False
  kernelExists <- fileExist (bPath <> "/vmlinuz")
  initrdExists <- fileExist (bPath <> "/initramfs.img")
  bEntryExists <- fileExist bEntry
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
  lFileExists <- fileExist lFile
  rDirExists <- fileExist rDir
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

haldPath :: FilePath
haldPath = "/.ald"

bootPath :: FilePath
bootPath = "/boot"

deployment :: String
deployment = "Deployment"
