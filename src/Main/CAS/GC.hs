module Main.CAS.GC (collectGarbage, restoreStoreFlags) where

import Control.Exception (IOException, catch)
import Control.Monad (unless, when)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Set qualified as Set
import Main.Config qualified as Config
import Main.Lock qualified as Lock
import Main.Util qualified as Util
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, removeDirectory, removeFile)
import System.FilePath ((</>))
import System.Posix.Files (deviceID, fileID, isDirectory, isRegularFile)
import System.Posix.Types (DeviceID, FileID)
import UnliftIO.Async (pooledForConcurrentlyN_, pooledForConcurrently_)
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
  refSetVar <- newIORef Set.empty
  pooledForConcurrently_ keptDepIds $ \depId -> do
    let dir = hp </> show depId </> "usr"
    dirExists <- doesDirectoryExist dir
    when dirExists $ walkTree dir refSetVar
  refSet <- readIORef refSetVar

  dirExists <- doesDirectoryExist casDir
  when dirExists $ do
    prefixes <- listDirectory casDir
    pooledForConcurrentlyN_ 2 prefixes $ \p -> do
      let casPath = casDir </> p
      objects <- listDirectory casPath
      pooledForConcurrentlyN_ workThreads objects $ \o -> do
        let casObj = casPath </> o
        mStat <- Util.tryStat casObj
        case mStat of
          Just stat -> do
            let ino = (deviceID stat, fileID stat)
            unless (ino `Set.member` refSet) $ do
              Lock.setMutable casObj
              catch
                (removeFile casObj)
                (\e -> let _ = show (e :: IOException) in return ())
          Nothing -> return ()
  removeEmptyDirectories casDir
  restoreStoreFlags conf

walkTree :: FilePath -> IORef (Set.Set (DeviceID, FileID)) -> IO ()
walkTree dir ref = do
  contents <- listDirectory dir
  pooledForConcurrentlyN_ 2 contents $ \name -> do
    let path = dir </> name
    mStat <- Util.tryStat path
    case mStat of
      Just stat
        | isRegularFile stat ->
            atomicModifyIORef' ref (\s -> (Set.insert (deviceID stat, fileID stat) s, ()))
        | isDirectory stat -> walkTree path ref
        | otherwise -> return ()
      Nothing -> return ()

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
