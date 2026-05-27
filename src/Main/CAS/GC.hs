module Main.CAS.GC (collectGarbage, restoreStoreFlags) where

import Control.Monad (unless, when)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Set qualified as Set
import Main.Config qualified as Config
import Main.Lock qualified as Lock
import Main.Util (TreeAction (..), WalkStrategy (..))
import Main.Util qualified as Util
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, removeDirectory, removeFile)
import System.FilePath ((</>))
import System.Posix.Files (deviceID, fileID, isRegularFile)
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
  pooledForConcurrently_ keptDepIds $ \depId ->
    Util.walk
      (ParallelN 2)
      ( TreeAction
          { dirAction = \_ _ -> return (),
            symAction = \_ _ -> return (),
            fileAction = \_ s ->
              when (isRegularFile s) $
                atomicModifyIORef' refSetVar (\acc -> (Set.insert (deviceID s, fileID s) acc, ()))
          }
      )
      (hp </> show depId </> "usr")
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
              Util.ioOrPass $ removeFile casObj
          Nothing -> return ()
  removeEmptyDirectories casDir

removeEmptyDirectories :: FilePath -> IO ()
removeEmptyDirectories = Util.walk (ParallelN 2) action
  where
    action =
      TreeAction
        { dirAction = \d _ -> do
            contents <- listDirectory d
            when (null contents) $
              Util.ioOrPass $
                removeDirectory d,
          symAction = \_ _ -> return (),
          fileAction = \_ _ -> return ()
        }
