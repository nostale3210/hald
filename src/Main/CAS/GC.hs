module Main.CAS.GC (collectGarbage, restoreStoreFlags) where

import Control.Concurrent.Async (mapConcurrently, mapConcurrently_)
import Control.Exception (IOException, catch)
import Control.Monad (forM_, unless, when)
import Data.Set qualified as Set
import Main.Config qualified as Config
import Main.Lock qualified as Lock
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, pathIsSymbolicLink, removeDirectory, removeFile)
import System.FilePath ((</>))
import System.Posix.Files (FileStatus, deviceID, fileID, getSymbolicLinkStatus)
import System.Posix.Types (DeviceID, FileID)

restoreStoreFlags :: Config.Config -> IO ()
restoreStoreFlags conf = do
  let casDir = Config.haldPath conf </> "objects"
  dirExists <- doesDirectoryExist casDir
  when dirExists $ do
    prefixes <- listDirectory casDir
    forM_ prefixes $ \p -> do
      let casPath = casDir </> p
      pExists <- doesDirectoryExist casPath
      when pExists $ Lock.setImmutable casPath
      objects <- listDirectory casPath
      mapConcurrently_
        ( \f -> do
            let casFile = casPath </> f
            fExists <- doesFileExist casFile
            when fExists $ Lock.setImmutable casFile
        )
        objects

collectGarbage :: Config.Config -> [Int] -> IO ()
collectGarbage conf keptDepIds = do
  let hp = Config.haldPath conf
      casDir = hp </> "objects"
  referenced <- concat <$> mapConcurrently (`walkDeployment` hp) keptDepIds
  let refSet = Set.fromList referenced
  hexPrefixes <- filter (\d -> length d == 2 && all (`elem` "0123456789abcdef") d) <$> listDirectory casDir
  forM_ hexPrefixes $ \p -> do
    let pPath = casDir </> p
    pExists <- doesDirectoryExist pPath
    when pExists $ Lock.setMutable pPath
  casFiles <- concat <$> mapConcurrently
    (\p -> map (\f -> casDir </> p </> f) <$> listDirectory (casDir </> p))
    hexPrefixes
  mapConcurrently_
    ( \casPath -> do
        mStat <- tryStat casPath
        case mStat of
          Just stat -> do
            let ino = (deviceID stat, fileID stat)
            unless (ino `Set.member` refSet) $ do
              Lock.setMutable casPath
              catch
                (removeFile casPath)
                (\e -> let _ = show (e :: IOException) in return ())
          Nothing -> return ()
    )
    casFiles
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
  results <- mapM (processEntry dir) contents
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
    forM_ contents $ \name -> do
      let subPath = dir </> name
      isDir <- doesDirectoryExist subPath
      when isDir $ removeEmptyDirectories subPath
    contents' <- listDirectory dir
    when (null contents') $
      catch
        (removeDirectory dir)
        (\e -> let _ = show (e :: IOException) in return ())
