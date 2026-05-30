module Hald.Legacy
  ( detectLegacyPaths,
    resolveRootDir,
    resolveLockfile,
    findDeploymentIds,
    readDepLockfile,
    treeRootDir,
    treeLockfile,
  )
where

import Data.Maybe (listToMaybe, mapMaybe, maybeToList)
import Data.Set qualified as Set
import Hald.Config qualified as Config
import Hald.Util qualified as Util
import System.Directory (canonicalizePath, doesDirectoryExist)
import System.FilePath ((</>))

detectLegacyPaths :: Config.Config -> IO Config.LegacyPaths
detectLegacyPaths conf = do
  aldExists <- doesDirectoryExist "/.ald"
  if not aldExists
    then return Config.noLegacyPaths
    else do
      aldReal <- canonicalizePath "/.ald"
      haldReal <- canonicalizePath (Config.haldPath conf)
      if aldReal == haldReal
        then return Config.noLegacyPaths
        else return (Config.LegacyPaths (Just "/.ald"))

treeRootDir :: Config.Config -> Int -> FilePath
treeRootDir conf depId = Config.haldPath conf </> "trees" </> show depId

treeLockfile :: Config.Config -> Int -> FilePath
treeLockfile conf depId = Config.haldPath conf </> "trees/." <> show depId

flatRootDir :: Config.Config -> Int -> FilePath
flatRootDir conf depId = Config.haldPath conf </> show depId

flatLockfile :: Config.Config -> Int -> FilePath
flatLockfile conf depId = Config.haldPath conf <> "/." <> show depId

legacyRootDir :: Config.LegacyPaths -> Int -> Maybe FilePath
legacyRootDir lp depId = (\p -> p </> show depId) <$> Config.legacyPath lp

legacyLockfile :: Config.LegacyPaths -> Int -> Maybe FilePath
legacyLockfile lp depId = (\p -> p <> "/." <> show depId) <$> Config.legacyPath lp

resolveRootDir :: Config.Config -> Int -> IO (Maybe FilePath)
resolveRootDir conf depId = do
  let candidates =
        treeRootDir conf depId
          : flatRootDir conf depId
          : maybeToList (legacyRootDir (Config.legacyPaths conf) depId)
  firstExists candidates

resolveLockfile :: Config.Config -> Int -> IO (Maybe FilePath)
resolveLockfile conf depId = do
  let candidates =
        treeLockfile conf depId
          : flatLockfile conf depId
          : maybeToList (legacyLockfile (Config.legacyPaths conf) depId)
  firstExists candidates

firstExists :: [FilePath] -> IO (Maybe FilePath)
firstExists [] = return Nothing
firstExists (p : ps) = do
  exists <- Util.pathExists p
  if exists then return (Just p) else firstExists ps

findDeploymentIds :: Config.Config -> IO [Int]
findDeploymentIds conf = do
  let hp = Config.haldPath conf
      lp = Config.legacyPaths conf
  treePaths <- Util.listDirSafe (hp </> "trees")
  hpPaths <- Util.listDirSafe hp
  let fromTree = extractIds treePaths
      fromFlat = extractIds hpPaths
      combined = Set.union fromTree fromFlat
  idsWithLegacy <-
    case Config.legacyPath lp of
      Nothing -> return combined
      Just lp' -> do
        legacyPaths' <- Util.listDirSafe lp'
        let fromLegacy = extractIds legacyPaths'
        return $ Set.union combined fromLegacy
  return $ Set.toList idsWithLegacy
  where
    extractIds paths =
      let dirSet =
            Set.fromList $
              mapMaybe (parseId . filter (`notElem` ['.', '/'])) $
                filter Util.startsWithDigit paths
          lfSet =
            Set.fromList $
              mapMaybe (parseId . filter (`notElem` ['.', '/'])) $
                filter Util.startsWithDotDigit paths
       in Set.union dirSet lfSet
    parseId s = fmap fst (listToMaybe . reads $ s)

readDepLockfile :: FilePath -> IO (Maybe Int)
readDepLockfile root = do
  let newLock = root <> "/usr/.hald_dep"
      oldLock = root <> "/usr/.ald_dep"
  newExists <- Util.pathExists newLock
  if newExists
    then Just . parseDepId <$> readFile newLock
    else do
      oldExists <- Util.pathExists oldLock
      if oldExists
        then Just . parseDepId <$> readFile oldLock
        else return Nothing
  where
    parseDepId content = maybe 0 fst (listToMaybe . reads . head . lines $ content)
