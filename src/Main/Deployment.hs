module Main.Deployment where

import Control.Exception (IOException, catch)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Set qualified as Set
import Main.Config qualified as Config
import Main.Util qualified as Util
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
          lockfile = Just (Config.haldPath conf <> "/." <> show depId),
          rootDir = Just (Config.haldPath conf </> show depId),
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
  let lFile = Config.haldPath conf <> "/." <> show depId
      rDir = Config.haldPath conf </> show depId
      markerFile = rDir <> "/.backend"
  lFileExists <- Util.pathExists lFile
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
        lockfile = if lFileExists then Just lFile else Nothing,
        rootDir = if rDirExists && rDirIsDir then Just rDir else Nothing,
        bootComponents = bComponents
      }

getDeployments :: Config.Config -> IO [FilePath]
getDeployments conf = do
  let hp = Config.haldPath conf
      bp = Config.bootPath conf
      ep = bp <> "/loader/entries"
      up = Config.ukiPath conf
      startsWithDigit (d : _) = d `elem` ['0' .. '9']
      startsWithDigit _ = False
      startsWithDotDigit ('.' : d : _) = d `elem` ['0' .. '9']
      startsWithDotDigit _ = False
      isNumericConf f =
        not (null f)
          && head f `elem` ['0' .. '9']
          && let l = length f in l >= 6 && drop (l - 5) f == ".conf"
  hpEntries <- Util.listDirSafe hp
  bdEntries <- Util.listDirSafe bp
  beEntries <- Util.listDirSafe ep
  ukiEntries <- Util.listDirSafe up
  let lockfiles = map (hp </>) $ filter startsWithDotDigit hpEntries
      rootDirs = map (hp </>) $ filter startsWithDigit hpEntries
      bootDirs = map (bp </>) $ filter startsWithDigit bdEntries
      bootEntrys = map (ep </>) $ filter isNumericConf beEntries
      ukis = map (up </>) $ filter startsWithDotDigit ukiEntries
      lockSet = Set.fromList $ map (Util.removeString "." . Util.removeString (hp <> "/")) lockfiles
      rootSet = Set.fromList $ map (Util.removeString (hp <> "/")) rootDirs
      bootDSet = Set.fromList $ map (Util.removeString (bp <> "/")) bootDirs
      bootESet = Set.fromList $ map (Util.removeString ".conf" . Util.removeString (ep <> "/")) bootEntrys
      ukiSet = Set.fromList $ map (Util.removeString ".efi" . Util.removeString (up <> "/")) ukis
  return $ Set.toList $ Set.union ukiSet $ Set.union bootESet . Set.union bootDSet . Set.union lockSet $ rootSet

getDeploymentsInt :: Config.Config -> IO [Int]
getDeploymentsInt conf =
  getDeployments conf >>= \deployments ->
    return $ mapMaybe (fmap fst . listToMaybe . reads) deployments

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
      case reads (head $ lines content) of
        [(n, "")] -> return n
        _ -> return 0
