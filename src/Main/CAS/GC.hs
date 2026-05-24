module Main.CAS.GC (collectGarbage, restoreStoreFlags) where

import Control.Exception (IOException, catch)
import Control.Monad (unless, when)
import Data.Set qualified as Set
import Main.Config qualified as Config
import Main.Lock qualified as Lock
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, removeDirectory, removeFile)
import System.FilePath ((</>))
import System.Posix.Files (FileStatus, deviceID, fileID, getSymbolicLinkStatus, isDirectory, isRegularFile)
import System.Posix.Types (DeviceID, FileID)
import UnliftIO.Async (pooledForConcurrently, pooledForConcurrentlyN, pooledForConcurrentlyN_, pooledForConcurrently_)
import UnliftIO.Concurrent (getNumCapabilities)

restoreStoreFlags :: Config.Config -> IO ()
restoreStoreFlags conf = do
  threads <- getNumCapabilities
  let casDir = Config.haldPath conf </> "objects"
      workThreads = max 1 $ div threads 2
  dirExists <- doesDirectoryExist casDir
  when dirExists $ do
    prefixes <- listDirectory casDir
    pooledForConcurrentlyN_ 2 prefixes $ \p -> do
      let casPath = casDir </> p
      pExists <- doesDirectoryExist casPath
      when pExists $ Lock.setImmutable casPath
      objects <- listDirectory casPath
      pooledForConcurrentlyN_ workThreads objects $ \o -> do
        let casObj = casPath </> o
        oExists <- doesFileExist casObj
        when oExists $ Lock.setImmutable casObj

collectGarbage :: Config.Config -> [Int] -> IO ()
collectGarbage conf keptDepIds = do
  threads <- getNumCapabilities
  let hp = Config.haldPath conf
      casDir = hp </> "objects"
      workThreads = max 1 $ div threads 2
  referenced <- fmap concat . pooledForConcurrently keptDepIds $ \depId -> do
    let dir = hp </> show depId </> "usr"
    dirExists <- doesDirectoryExist dir
    if dirExists then walkTree dir else return []
  let refSet = Set.fromList referenced
  dirExists <- doesDirectoryExist casDir
  when dirExists $ do
    prefixes <- listDirectory casDir
    pooledForConcurrentlyN_ 2 prefixes $ \p -> do
      let casPath = casDir </> p
      objects <- listDirectory casPath
      pooledForConcurrentlyN_ workThreads objects $ \o -> do
        mStat <- tryStat o
        case mStat of
          Just stat -> do
            let ino = (deviceID stat, fileID stat)
            unless (ino `Set.member` refSet) $ do
              Lock.setMutable o
              catch
                (removeFile o)
                (\e -> let _ = show (e :: IOException) in return ())
          Nothing -> return ()
  removeEmptyDirectories casDir
  restoreStoreFlags conf

walkTree :: FilePath -> IO [(DeviceID, FileID)]
walkTree dir = do
  contents <- listDirectory dir
  fmap concat . pooledForConcurrentlyN 2 contents $ \name -> do
    let path = dir </> name
    mStat <- tryStat path
    case mStat of
      Just stat
        | isRegularFile stat -> return [(deviceID stat, fileID stat)]
        | isDirectory stat -> walkTree path
        | otherwise -> return []
      Nothing -> return []

tryStat :: FilePath -> IO (Maybe FileStatus)
tryStat path =
  catch
    (Just <$> getSymbolicLinkStatus path)
    (\e -> let _ = show (e :: IOException) in return Nothing)

removeEmptyDirectories :: FilePath -> IO ()
removeEmptyDirectories dir = do
  dirExists <- doesDirectoryExist dir
  when dirExists $ do
    contents <- listDirectory dir
    pooledForConcurrently_ contents $ \name -> do
      let subPath = dir </> name
      isDir <- doesDirectoryExist subPath
      when isDir $ removeEmptyDirectories subPath
    contents' <- listDirectory dir
    when (null contents') $
      catch
        (removeDirectory dir)
        (\e -> let _ = show (e :: IOException) in return ())
