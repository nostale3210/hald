module Main.CAS.Ingest
  ( TreeEntry (..),
    AssetMap,
    ingestPath,
    deployTree,
  )
where

import Control.Exception (IOException, catch)
import Control.Monad (unless)
import Data.Map.Strict qualified as Map
import Main.CAS.Hash qualified as Hash
import Main.Lock qualified as Lock
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory, pathIsSymbolicLink)
import System.FilePath (takeDirectory, (</>))
import System.Posix.Files (createLink, createSymbolicLink, readSymbolicLink)
import UnliftIO.Async (pooledMapConcurrently, pooledMapConcurrentlyN, pooledMapConcurrently_)

data TreeEntry
  = TreeDir
  | TreeSymlink FilePath
  | TreeFile FilePath

type AssetMap = Map.Map FilePath TreeEntry

ingestPath :: FilePath -> FilePath -> IO AssetMap
ingestPath srcDir casDir = do
  entries <- buildIndex srcDir srcDir
  resolveEntries srcDir casDir entries

buildIndex :: FilePath -> FilePath -> IO (Map.Map FilePath TreeEntry)
buildIndex rootDir currentDir = do
  contents <- listDirectory currentDir
  entries <- pooledMapConcurrentlyN 2 process contents
  return $ Map.unions entries
  where
    process name = do
      let fullPath = currentDir </> name
          relPath = makeRelative rootDir fullPath
      isDir <- doesDirectoryExist fullPath
      isLink <- pathIsSymbolicLink fullPath
      if isDir && not isLink
        then do
          sub <- buildIndex rootDir fullPath
          return $ Map.insert relPath TreeDir sub
        else
          if isLink
            then do
              target <- readSymbolicLink fullPath
              return $ Map.singleton relPath (TreeSymlink target)
            else
              return $ Map.singleton relPath (TreeFile "")

resolveEntries :: FilePath -> FilePath -> AssetMap -> IO AssetMap
resolveEntries srcDir casDir =
  fmap Map.fromList . pooledMapConcurrently resolveEntry . Map.toList
  where
    resolveEntry (relPath, entry) = case entry of
      TreeFile "" -> (relPath,) . TreeFile <$> doHash (srcDir </> relPath) casDir
      _ -> return (relPath, entry)

doHash :: FilePath -> FilePath -> IO FilePath
doHash srcPath casDir = do
  hashStr <- Hash.hashFile srcPath
  let prefix = take 2 hashStr
      destDir = casDir </> prefix
      destPath = destDir </> hashStr
  createDirectoryIfMissing True destDir
  Lock.setMutable destDir
  destExists <- doesFileExist destPath
  unless destExists $ do
    catch
      (copyFile srcPath destPath)
      (\e -> let _ = show (e :: IOException) in return ())
    Lock.setImmutable destPath
  return destPath

deployTree :: FilePath -> AssetMap -> IO ()
deployTree targetRoot assetMap = do
  let casPaths = [p | TreeFile p <- Map.elems assetMap]
  pooledMapConcurrently_ Lock.setMutable casPaths
  createDirectoryIfMissing True targetRoot
  pooledMapConcurrently_ (deployEntry targetRoot) (Map.toList assetMap)
  pooledMapConcurrently_ Lock.setImmutable casPaths

makeRelative :: FilePath -> FilePath -> FilePath
makeRelative root path =
  case stripPrefix root path of
    Just ('/' : rest) -> rest
    Just rest -> rest
    Nothing -> path

stripPrefix :: (Eq a) => [a] -> [a] -> Maybe [a]
stripPrefix [] ys = Just ys
stripPrefix (x : xs) (y : ys)
  | x == y = stripPrefix xs ys
stripPrefix _ _ = Nothing

deployEntry :: FilePath -> (FilePath, TreeEntry) -> IO ()
deployEntry targetRoot (relPath, entry) = case entry of
  TreeDir -> createDirectoryIfMissing True (targetRoot </> relPath)
  TreeSymlink target -> do
    createDirectoryIfMissing True (takeDirectory (targetRoot </> relPath))
    createSymbolicLink target (targetRoot </> relPath)
  TreeFile casPath -> do
    let targetPath = targetRoot </> relPath
    targetExists <- doesFileExist targetPath
    unless targetExists $ do
      createDirectoryIfMissing True (takeDirectory targetPath)
      createLink casPath targetPath
