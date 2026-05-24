module Main.CAS.Ingest
  ( TreeEntry (..),
    AssetMap,
    ingestPath,
    deployTree,
    saveAssetMap,
    loadAssetMap,
  )
where

import Control.Exception (IOException, catch)
import Control.Monad (unless)
import Data.HashMap.Strict qualified as HashMap
import Main.CAS.Hash qualified as Hash
import Main.Lock qualified as Lock
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, doesPathExist, listDirectory, pathIsSymbolicLink)
import System.FilePath (takeDirectory, (</>))
import System.Posix.Files (createLink, createSymbolicLink, readSymbolicLink)
import UnliftIO.Async (pooledMapConcurrently, pooledMapConcurrentlyN, pooledMapConcurrently_)

data TreeEntry
  = TreeDir
  | TreeSymlink FilePath
  | TreeFile FilePath

type AssetMap = HashMap.HashMap FilePath TreeEntry

ingestPath :: FilePath -> FilePath -> IO AssetMap
ingestPath srcDir casDir = do
  entries <- buildIndex srcDir srcDir
  resolveEntries srcDir casDir entries

buildIndex :: FilePath -> FilePath -> IO AssetMap
buildIndex rootDir currentDir = do
  contents <- listDirectory currentDir
  entries <- pooledMapConcurrentlyN 2 process contents
  return $ HashMap.unions entries
  where
    process name = do
      let fullPath = currentDir </> name
          relPath = makeRelative rootDir fullPath
      isDir <- doesDirectoryExist fullPath
      isLink <- pathIsSymbolicLink fullPath
      if isDir && not isLink
        then do
          sub <- buildIndex rootDir fullPath
          return $ HashMap.insert relPath TreeDir sub
        else
          if isLink
            then do
              target <- readSymbolicLink fullPath
              return $ HashMap.singleton relPath (TreeSymlink target)
            else
              return $ HashMap.singleton relPath (TreeFile "")

resolveEntries :: FilePath -> FilePath -> AssetMap -> IO AssetMap
resolveEntries srcDir casDir =
  fmap HashMap.fromList . pooledMapConcurrently resolveEntry . HashMap.toList
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
  return (prefix </> hashStr)

deployTree :: FilePath -> FilePath -> AssetMap -> IO ()
deployTree casDir targetRoot assetMap = do
  let entries = HashMap.toList assetMap
      casPaths = [casDir </> p | (_, TreeFile p) <- entries]
  pooledMapConcurrently_ Lock.setMutable casPaths
  createDirectoryIfMissing True targetRoot
  pooledMapConcurrently_ (deployEntry casDir targetRoot) entries
  pooledMapConcurrently_ Lock.setImmutable casPaths

saveAssetMap :: FilePath -> AssetMap -> IO ()
saveAssetMap path assetMap = do
  lines' <- pooledMapConcurrently formatEntry (HashMap.toList assetMap)
  writeFile path (unlines lines')

loadAssetMap :: FilePath -> IO AssetMap
loadAssetMap path = do
  content <- readFile path
  entries <- pooledMapConcurrently parseEntry (lines content)
  return (HashMap.fromList entries)

formatEntry :: (FilePath, TreeEntry) -> IO String
formatEntry (relPath, entry) = return $ case entry of
  TreeDir -> "D\t" <> relPath
  TreeSymlink target -> "S\t" <> relPath <> "\t" <> target
  TreeFile casRelPath -> "F\t" <> relPath <> "\t" <> casRelPath

parseEntry :: String -> IO (FilePath, TreeEntry)
parseEntry line = return $ case words line of
  ["D", relPath] -> (relPath, TreeDir)
  ["S", relPath, target] -> (relPath, TreeSymlink target)
  ["F", relPath, casRelPath] -> (relPath, TreeFile casRelPath)
  _ -> error $ "Invalid AssetMap entry: " <> line

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

deployEntry :: FilePath -> FilePath -> (FilePath, TreeEntry) -> IO ()
deployEntry casDir targetRoot (relPath, entry) = case entry of
  TreeDir -> createDirectoryIfMissing True (targetRoot </> relPath)
  TreeSymlink target -> do
    let targetPath = targetRoot </> relPath
    targetExists <- doesPathExist targetPath
    unless targetExists $ do
      createDirectoryIfMissing True (takeDirectory targetPath)
      createSymbolicLink target targetPath
  TreeFile casRelPath -> do
    let casPath = casDir </> casRelPath
        targetPath = targetRoot </> relPath
    targetExists <- doesFileExist targetPath
    unless targetExists $ do
      createDirectoryIfMissing True (takeDirectory targetPath)
      createLink casPath targetPath
