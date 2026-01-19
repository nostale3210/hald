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
    bootEntry :: Maybe FilePath,
    ukiPath :: Maybe FilePath
  }
  deriving (Show, Eq)

createDeployment :: [Int] -> Config.Config -> Deployment
createDeployment exDeps conf =
  let depId = Util.newIdentifier exDeps
   in Deployment
        { identifier = depId,
          lockfile = Just (Config.haldPath conf <> "/." <> show depId),
          rootDir = Just (Config.haldPath conf <> "/" <> show depId),
          bootComponents = createBootPaths depId conf
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
            bootEntry = Nothing,
            ukiPath = Nothing
          }
    }

createBootPaths :: Int -> Config.Config -> BootComponents
createBootPaths depId conf =
  BootComponents
    { bootDir = Just (Config.bootPath conf <> "/" <> show depId),
      bootEntry = Just (Config.bootPath conf <> "/loader/entries/" <> show depId <> ".conf"),
      ukiPath = Just (Config.ukiPath conf <> "/" <> show depId <> ".efi")
    }

getBootComponents :: Int -> Config.Config -> IO BootComponents
getBootComponents depId conf = do
  let bPath = Config.bootPath conf <> "/" <> show depId
      bEntry = Config.bootPath conf <> "/loader/entries/" <> show depId <> ".conf"
      uki = Config.ukiPath conf <> "/" <> show depId <> ".efi"
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
  let lFile = Config.haldPath conf <> "/." <> show depId
      rDir = Config.haldPath conf <> "/" <> show depId
  lFileExists <- Util.pathExists lFile
  rDirExists <- Util.pathExists rDir
  rDirIsDir <-
    if rDirExists
      then do
        doesDirectoryExist rDir
      else return False
  bComponents <- getBootComponents depId conf
  return
    Deployment
      { identifier = depId,
        lockfile = if lFileExists then Just lFile else Nothing,
        rootDir = if rDirExists && rDirIsDir then Just rDir else Nothing,
        bootComponents = bComponents
      }

getDeployments :: Config.Config -> IO [FilePath]
getDeployments conf = do
  lockfiles <- globDir1 (compile ".[0-9]*") (Config.haldPath conf)
  rootDirs <- globDir1 (compile "[0-9]*") (Config.haldPath conf)
  bootDirs <- globDir1 (compile "[0-9]*") (Config.bootPath conf)
  bootEntrys <- globDir1 (compile "[0-9]*.conf") (Config.bootPath conf <> "/loader/entries")
  ukis <- globDir1 (compile ".[0-9]*") (Config.ukiPath conf)
  let lockSet = Set.fromList $ map (Util.removeString "." . Util.removeString (Config.haldPath conf <> "/")) lockfiles
      rootSet = Set.fromList $ map (Util.removeString (Config.haldPath conf <> "/")) rootDirs
      bootDSet = Set.fromList $ map (Util.removeString (Config.bootPath conf <> "/")) bootDirs
      bootESet = Set.fromList $ map (Util.removeString ".conf" . Util.removeString (Config.bootPath conf <> "/loader/entries/")) bootEntrys
      ukiSet = Set.fromList $ map (Util.removeString ".efi" . Util.removeString (Config.ukiPath conf)) ukis
  return $ Set.toList $ Set.union ukiSet $ Set.union bootESet . Set.union bootDSet . Set.union lockSet $ rootSet

getDeploymentsInt :: Config.Config -> IO [Int]
getDeploymentsInt conf =
  getDeployments conf >>= \deployments ->
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
