module Main.CAS.Ingest
  ( TreeEntry (..),
    AssetMap,
    ingestTree,
    deployTreeFromFile,
    loadAssetMap,
  )
where

import Control.Monad (forM, forM_, unless)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.HashMap.Strict qualified as HashMap
import Main.CAS.Hash qualified as Hash
import Main.Lock qualified as Lock
import Main.Util qualified as Util
import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist, doesPathExist, listDirectory)
import System.FilePath (takeDirectory, (</>))
import System.IO (Handle, IOMode (WriteMode), hClose, hPutStrLn, openFile)
import System.Posix.Files (createLink, createSymbolicLink, isDirectory, isRegularFile, isSymbolicLink, readSymbolicLink)
import UnliftIO.Async (pooledMapConcurrently, pooledMapConcurrently_)

data TreeEntry
  = TreeDir
  | TreeSymlink !B8.ByteString
  | TreeFile !B8.ByteString

type AssetMap = HashMap.HashMap FilePath TreeEntry

ingestTree :: FilePath -> FilePath -> FilePath -> IO ()
ingestTree srcDir casDir outputPath = do
  h <- openFile outputPath WriteMode
  walkDirectory h srcDir srcDir casDir
  hClose h

walkDirectory :: Handle -> FilePath -> FilePath -> FilePath -> IO ()
walkDirectory h rootDir currentDir casDir = do
  contents <- listDirectory currentDir
  classified <- forM contents $ \name -> do
    let fullPath = currentDir </> name
        relPath = makeRelative rootDir fullPath
    mStat <- Util.tryStat fullPath
    return (fullPath, relPath, mStat)

  let dirs = [(fp, rp) | (fp, rp, Just s) <- classified, isDirectory s]
      files = [(fp, rp) | (fp, rp, Just s) <- classified, isRegularFile s]
      syms = [(fp, rp) | (fp, rp, Just s) <- classified, isSymbolicLink s]
      special = [(fp, rp) | (fp, rp, Just s) <- classified, not (isDirectory s || isRegularFile s || isSymbolicLink s)]

  forM_ dirs $ \(fullPath, relPath) -> do
    hPutStrLn h $ "D\t" <> relPath
    walkDirectory h rootDir fullPath casDir

  hashedFiles <- pooledMapConcurrently (\(fp, rp) -> doHash fp casDir >>= \cp -> return (rp, cp)) files
  forM_ hashedFiles $ \(relPath, casPath) ->
    hPutStrLn h $ "F\t" <> relPath <> "\t" <> casPath

  forM_ syms $ \(fullPath, relPath) -> do
    target <- readSymbolicLink fullPath
    hPutStrLn h $ "S\t" <> relPath <> "\t" <> target

  hashedSpecial <- pooledMapConcurrently (\(fp, rp) -> doHash fp casDir >>= \cp -> return (rp, cp)) special
  forM_ hashedSpecial $ \(relPath, casPath) ->
    hPutStrLn h $ "F\t" <> relPath <> "\t" <> casPath

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
    Util.ioOrPass $ copyFile srcPath destPath
    Lock.setImmutable destPath
  return (prefix </> hashStr)

deployTreeFromFile :: FilePath -> FilePath -> FilePath -> IO ()
deployTreeFromFile casDir targetRoot assetMapFile = do
  content <- BS.readFile assetMapFile
  let entries = parseLines content
  pooledMapConcurrently_ (setMutableIfFile casDir) entries
  createDirectoryIfMissing True targetRoot
  pooledMapConcurrently_ (deployEntry casDir targetRoot) entries
  pooledMapConcurrently_ (setImmutableIfFile casDir) entries

parseLines :: BS.ByteString -> [(B8.ByteString, TreeEntry)]
parseLines = map parseEntry . B8.lines

parseEntry :: B8.ByteString -> (B8.ByteString, TreeEntry)
parseEntry line = case B8.split '\t' line of
  [d, p] | d == B8.singleton 'D' -> (p, TreeDir)
  [s, p, t] | s == B8.singleton 'S' -> (p, TreeSymlink t)
  [f, p, c] | f == B8.singleton 'F' -> (p, TreeFile c)
  _ -> error $ "Invalid AssetMap entry: " <> B8.unpack line

setMutableIfFile :: FilePath -> (B8.ByteString, TreeEntry) -> IO ()
setMutableIfFile casDir (_, TreeFile p) = Lock.setMutable (casDir </> B8.unpack p)
setMutableIfFile _ _ = return ()

setImmutableIfFile :: FilePath -> (B8.ByteString, TreeEntry) -> IO ()
setImmutableIfFile casDir (_, TreeFile p) = Lock.setImmutable (casDir </> B8.unpack p)
setImmutableIfFile _ _ = return ()

loadAssetMap :: FilePath -> IO AssetMap
loadAssetMap path = do
  content <- BS.readFile path
  return $ HashMap.fromList [(B8.unpack p, e) | (p, e) <- parseLines content]

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

deployEntry :: FilePath -> FilePath -> (B8.ByteString, TreeEntry) -> IO ()
deployEntry casDir targetRoot (relPath, entry) = case entry of
  TreeDir -> createDirectoryIfMissing True (targetRoot </> B8.unpack relPath)
  TreeSymlink target -> do
    let targetPath = targetRoot </> B8.unpack relPath
    targetExists <- doesPathExist targetPath
    unless targetExists $ do
      createDirectoryIfMissing True (takeDirectory targetPath)
      createSymbolicLink (B8.unpack target) targetPath
  TreeFile casRelPath -> do
    let casPath = casDir </> B8.unpack casRelPath
        targetPath = targetRoot </> B8.unpack relPath
    targetExists <- doesFileExist targetPath
    unless targetExists $ do
      createDirectoryIfMissing True (takeDirectory targetPath)
      createLink casPath targetPath
