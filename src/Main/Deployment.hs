module Main.Deployment where

import Control.Exception (IOException, catch)
import Data.Set qualified as Set
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

createDeployment :: [Int] -> FilePath -> FilePath -> Deployment
createDeployment exDeps hp bp =
  let depId = Util.newIdentifier exDeps
   in Deployment
        { identifier = depId,
          lockfile = Just (hp <> "/." <> show depId),
          rootDir = Just (hp <> "/" <> show depId),
          bootComponents = createBootPaths depId bp
        }

dummyDeployment :: Deployment
dummyDeployment =
  Deployment
    { identifier = -1,
      lockfile = Nothing,
      rootDir = Nothing,
      bootComponents =
        BootComponents
          { bootDir = Nothing,
            bootEntry = Nothing
          }
    }

createBootPaths :: Int -> FilePath -> BootComponents
createBootPaths depId bp =
  BootComponents
    { bootDir = Just (bp <> "/" <> show depId),
      bootEntry = Just (bp <> "/loader/entries/" <> show depId <> ".conf")
    }

getBootComponents :: Int -> FilePath -> IO BootComponents
getBootComponents depId bp = do
  let bPath = bp <> "/" <> show depId
      bEntry = bp <> "/loader/entries/" <> show depId <> ".conf"
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

getDeployment :: Int -> FilePath -> FilePath -> FilePath -> IO Deployment
getDeployment depId root hp bp = do
  currentDepId <- getCurrentDeploymentId root
  let lFile = hp <> "/." <> show depId
      rDir = if depId == currentDepId then "/usr" else hp <> "/" <> show depId
  lFileExists <- Util.pathExists lFile
  rDirExists <- Util.pathExists rDir
  rDirIsDir <-
    if rDirExists
      then do
        doesDirectoryExist rDir
      else return False
  bComponents <- getBootComponents depId bp
  return
    Deployment
      { identifier = depId,
        lockfile = if lFileExists then Just lFile else Nothing,
        rootDir = if rDirExists && rDirIsDir then Just rDir else Nothing,
        bootComponents = bComponents
      }

getDeployments :: FilePath -> FilePath -> IO [FilePath]
getDeployments hp bp = do
  lockfiles <- globDir1 (compile ".[0-9]*") hp
  rootDirs <- globDir1 (compile "[0-9]*") hp
  bootDirs <- globDir1 (compile "[0-9]*") bp
  bootEntrys <- globDir1 (compile "[0-9]*.conf") (bp <> "/loader/entries")
  let lockSet = Set.fromList $ map (Util.removeString "." . Util.removeString (hp <> "/")) lockfiles
      rootSet = Set.fromList $ map (Util.removeString (hp <> "/")) rootDirs
      bootDSet = Set.fromList $ map (Util.removeString (bp <> "/")) bootDirs
      bootESet = Set.fromList $ map (Util.removeString ".conf" . Util.removeString (bp <> "/loader/entries/")) bootEntrys
  return $ Set.toList $ Set.union bootESet . Set.union bootDSet . Set.union lockSet $ rootSet

getDeploymentsInt :: FilePath -> FilePath -> IO [Int]
getDeploymentsInt hp bp =
  getDeployments hp bp >>= \deployments ->
    let intdeps = map (\d -> read d :: Int) deployments
     in return intdeps

getCurrentDeploymentId :: FilePath -> IO Int
getCurrentDeploymentId root =
  catch
    (readFile (root <> "/usr/.ald_dep"))
    ( \e ->
        let err = show (e :: IOException)
         in Util.printInfo ("Couldn't read deployment ID; " <> err) False
              >> return "0"
    )
    >>= \content ->
      return (read (head $ lines content) :: Int)

getDeploymentId :: Int -> FilePath -> IO Int
getDeploymentId depId hp =
  readFile (hp <> "/" <> show depId <> "/usr/.ald_dep") >>= \content ->
    return (read (head $ lines content) :: Int)
