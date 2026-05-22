module Main.CAS.GC (collectGarbage, restoreStoreFlags) where

import Control.Exception (IOException, catch)
import Control.Monad (unless, when)
import Data.Set qualified as Set
import Main.Config qualified as Config
import Main.Lock qualified as Lock
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, pathIsSymbolicLink, removeDirectory, removeFile)
import System.FilePath ((</>))
import System.Posix.Files (FileStatus, deviceID, fileID, getSymbolicLinkStatus)
import System.Posix.Types (DeviceID, FileID)
import UnliftIO.Async (mapConcurrently, pooledForConcurrentlyN, pooledForConcurrentlyN_)
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
  let casDir = Config.haldPath conf </> "objects"
      workThreads = max 1 $ div threads 2
  referenced <- concat <$> mapConcurrently (`walkDeployment` Config.haldPath conf) keptDepIds
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

walkDeployment :: Int -> FilePath -> IO [(DeviceID, FileID)]
walkDeployment depId hp = do
  let dir = hp </> show depId </> "usr"
  dirExists <- doesDirectoryExist dir
  if dirExists then walkTree dir else return []

walkTree :: FilePath -> IO [(DeviceID, FileID)]
walkTree dir = do
  contents <- listDirectory dir
  results <- pooledForConcurrentlyN 2 contents $ processEntry dir
  return $ concat results
  where
    processEntry parent name = do
      let path = parent </> name
      isDir <- doesDirectoryExist path
      isLink <- pathIsSymbolicLink path
      if isDir && not isLink
        then walkTree path
        else
          if isLink
            then return []
            else do
              mStat <- tryStat path
              case mStat of
                Just stat -> return [(deviceID stat, fileID stat)]
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
    pooledForConcurrentlyN_ 2 contents $ \name -> do
      let subPath = dir </> name
      isDir <- doesDirectoryExist subPath
      when isDir $ removeEmptyDirectories subPath
    contents' <- listDirectory dir
    when (null contents') $
      catch
        (removeDirectory dir)
        (\e -> let _ = show (e :: IOException) in return ())
