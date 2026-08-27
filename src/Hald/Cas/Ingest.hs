module Hald.Cas.Ingest
  ( ingestTree,
    deployTreeFromFile,
  )
where

import Control.Monad (forM, forM_, unless)
import Data.ByteString.Char8 qualified as B8
import Data.HashMap.Strict qualified as HashMap
import Data.List (partition)
import Data.Maybe (mapMaybe)
import Hald.Cas.AssetMap (TreeEntry (..))
import Hald.Cas.AssetMap qualified as AssetMap
import Hald.Cas.Hash qualified as Hash
import Hald.Container (findInLayers)
import Hald.Lock qualified as Lock
import Hald.Util qualified as Util
import System.Directory (copyFileWithMetadata, createDirectoryIfMissing, doesFileExist, doesPathExist, listDirectory, removeFile, renameFile)
import System.FilePath (makeRelative, takeDirectory, (</>))
import System.IO (Handle, IOMode (WriteMode), hClose, hPutStrLn, openTempFile, withFile)
import System.Posix.Files (createLink, createSymbolicLink, fileSize, isDirectory, isRegularFile, isSymbolicLink, readSymbolicLink)
import UnliftIO.Async (pooledMapConcurrently, pooledMapConcurrently_)

ingestTree :: FilePath -> FilePath -> FilePath -> FilePath -> [FilePath] -> IO ()
ingestTree containerRoot subDir casDir outputPath layerDiffs =
  withFile outputPath WriteMode $ \h ->
    walkDirectory h srcDir srcDir casDir subDir layerDiffs
  where
    srcDir = containerRoot </> subDir

walkDirectory :: Handle -> FilePath -> FilePath -> FilePath -> FilePath -> [FilePath] -> IO ()
walkDirectory h rootDir currentDir casDir subDir layerDiffs = do
  contents <- listDirectory currentDir
  classified <- forM contents $ \name -> do
    let fullPath = currentDir </> name
        relPath = makeRelative rootDir fullPath
    mStat <- Util.tryStat fullPath
    return (fullPath, relPath, mStat)

  let valid = mapMaybe (\(fp, rp, ms) -> (fp,rp,) <$> ms) classified
      (dirs3, rest1) = partition (\(_, _, s) -> isDirectory s) valid
      (files3, rest2) = partition (\(_, _, s) -> isRegularFile s) rest1
      (syms3, special3) = partition (\(_, _, s) -> isSymbolicLink s) rest2
      isEmpty (_, _, s) = fileSize s == 0
      (emptyFiles3, realFiles3) = partition isEmpty files3
      (emptySpecial3, realSpecial3) = partition isEmpty special3
      dropStatus = map (\(fp, rp, _) -> (fp, rp))
      relOf (_, rp, _) = rp
      dirs = dropStatus dirs3
      files = dropStatus realFiles3
      syms = dropStatus syms3
      special = dropStatus realSpecial3

  forM_ dirs $ \(fullPath, relPath) -> do
    hPutStrLn h $ "D\t" <> relPath
    walkDirectory h rootDir fullPath casDir subDir layerDiffs

  forM_ (map relOf emptyFiles3 ++ map relOf emptySpecial3) $ \relPath ->
    hPutStrLn h $ "E\t" <> relPath

  hashedFiles <-
    pooledMapConcurrently
      ( \(fp, rp) ->
          (rp,) <$> doHash fp rootDir subDir layerDiffs casDir
      )
      files
  forM_ hashedFiles $ \(relPath, casPath) ->
    hPutStrLn h $ "F\t" <> relPath <> "\t" <> casPath

  forM_ syms $ \(fullPath, relPath) -> do
    target <- readSymbolicLink fullPath
    hPutStrLn h $ "S\t" <> relPath <> "\t" <> target

  hashedSpecial <-
    pooledMapConcurrently
      ( \(fp, rp) ->
          (rp,) <$> doHash fp rootDir subDir layerDiffs casDir
      )
      special
  forM_ hashedSpecial $ \(relPath, casPath) ->
    hPutStrLn h $ "F\t" <> relPath <> "\t" <> casPath

doHash :: FilePath -> FilePath -> FilePath -> [FilePath] -> FilePath -> IO FilePath
doHash srcPath rootDir subDir layerDiffs casDir = do
  hashStr <- Hash.hashFile srcPath
  let prefix = take 2 hashStr
      destDir = casDir </> prefix
      destPath = destDir </> hashStr
  createDirectoryIfMissing True destDir
  Lock.setMutable destDir
  destExists <- doesFileExist destPath
  unless destExists $ do
    (tmpPath, tmpHandle) <- openTempFile destDir ".hald_tmp"
    hClose tmpHandle
    let relPath = makeRelative rootDir srcPath
        layerPath = subDir </> relPath
    mBacking <- findInLayers layerDiffs layerPath
    case mBacking of
      Just backing -> do
        ok <- Lock.ficlone backing tmpPath
        if ok
          then Lock.copyMetadata srcPath tmpPath
          else Util.ioOrPass $ copyFileWithMetadata srcPath tmpPath
      Nothing -> Util.ioOrPass $ copyFileWithMetadata srcPath tmpPath
    renameResult <- Util.safeCall $ renameFile tmpPath destPath
    case renameResult of
      Just _ -> do
        Lock.enableFsVerity destPath
        Lock.setImmutable destPath
      Nothing -> removeFile tmpPath
  return (prefix </> hashStr)

deployTreeFromFile :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
deployTreeFromFile casDir targetRoot emptyFile assetMapPath = do
  mAssetMap <- AssetMap.loadAssetMap assetMapPath
  case mAssetMap of
    Nothing -> Util.fatal ("Invalid entry in assetmap " <> assetMapPath)
    Just assetMap -> do
      let entries = [(B8.unpack p, e) | (p, e) <- HashMap.toList assetMap]
      pooledMapConcurrently_ (setFlagIfFile Lock.setMutable casDir) entries
      createDirectoryIfMissing True targetRoot
      pooledMapConcurrently_ (deployEntry casDir targetRoot emptyFile) entries
      pooledMapConcurrently_ (setFlagIfFile Lock.setImmutable casDir) entries

setFlagIfFile :: (FilePath -> IO ()) -> FilePath -> (FilePath, TreeEntry) -> IO ()
setFlagIfFile flag casDir entry = case entry of
  (_, TreeFile p) -> flag $ casDir </> B8.unpack p
  _ -> return ()

ensureAbsentThen :: (FilePath -> IO Bool) -> FilePath -> IO () -> IO ()
ensureAbsentThen exists targetPath action = do
  present <- exists targetPath
  unless present $ do
    createDirectoryIfMissing True (takeDirectory targetPath)
    action

deployEntry :: FilePath -> FilePath -> FilePath -> (FilePath, TreeEntry) -> IO ()
deployEntry casDir targetRoot emptyFile (relPath, entry) = case entry of
  TreeDir -> createDirectoryIfMissing True (targetRoot </> relPath)
  TreeSymlink target ->
    let targetPath = targetRoot </> relPath
     in ensureAbsentThen doesPathExist targetPath $
          createSymbolicLink (B8.unpack target) targetPath
  TreeFile casRelPath ->
    let targetPath = targetRoot </> relPath
     in ensureAbsentThen doesFileExist targetPath $
          createLink (casDir </> B8.unpack casRelPath) targetPath
  TreeEmpty ->
    let targetPath = targetRoot </> relPath
     in ensureAbsentThen doesFileExist targetPath $
          createLink emptyFile targetPath
