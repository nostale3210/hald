module Main.CAS.Ingest
  ( TreeEntry (..),
    AssetMap,
    ingestTree,
    deployTreeFromFile,
    loadAssetMap,
  )
where

import Control.Exception (IOException, catch)
import Control.Monad (forM, forM_, unless)
import Data.HashMap.Strict qualified as HashMap
import Main.CAS.Hash qualified as Hash
import Main.Lock qualified as Lock
import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist, doesPathExist, listDirectory)
import System.FilePath (takeDirectory, (</>))
import System.IO (Handle, IOMode (WriteMode), hClose, hPutStrLn, openFile)
import System.Posix.Files (createLink, createSymbolicLink, getSymbolicLinkStatus, isDirectory, isRegularFile, isSymbolicLink, readSymbolicLink)
import UnliftIO.Async (pooledMapConcurrently, pooledMapConcurrently_)

data TreeEntry
  = TreeDir
  | TreeSymlink FilePath
  | TreeFile FilePath

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
    mStat <-
      catch
        (Just <$> getSymbolicLinkStatus fullPath)
        (\e -> let _ = show (e :: IOException) in return Nothing)
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
    catch
      (copyFile srcPath destPath)
      (\e -> let _ = show (e :: IOException) in return ())
    Lock.setImmutable destPath
  return (prefix </> hashStr)

deployTreeFromFile :: FilePath -> FilePath -> FilePath -> IO ()
deployTreeFromFile casDir targetRoot assetMapFile = do
  content <- readFile assetMapFile
  let entries = parseLines content
      casPaths = [casDir </> p | (_, TreeFile p) <- entries]
  pooledMapConcurrently_ Lock.setMutable casPaths
  createDirectoryIfMissing True targetRoot
  pooledMapConcurrently_ (deployEntry casDir targetRoot) entries
  pooledMapConcurrently_ Lock.setImmutable casPaths

parseLines :: String -> [(FilePath, TreeEntry)]
parseLines = map parseEntry . lines

loadAssetMap :: FilePath -> IO AssetMap
loadAssetMap path = do
  content <- readFile path
  return $ HashMap.fromList (parseLines content)

parseEntry :: String -> (FilePath, TreeEntry)
parseEntry line = case splitOn '\t' line of
  ["D", relPath] -> (relPath, TreeDir)
  ["S", relPath, target] -> (relPath, TreeSymlink target)
  ["F", relPath, casRelPath] -> (relPath, TreeFile casRelPath)
  _ -> error $ "Invalid AssetMap entry: " <> line

splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn c s = let (before, after) = break (== c) s
              in before : case after of
                   ""       -> []
                   (_:rest) -> splitOn c rest

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
