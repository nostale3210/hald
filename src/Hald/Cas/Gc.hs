module Hald.Cas.Gc (collectGarbage, restoreStoreFlags, enableFsVerityOnCas) where

import Control.Concurrent.STM (atomically, modifyTVar', newTVarIO, readTVarIO)
import Control.Exception (IOException, bracket_, catch)
import Control.Monad (unless, when)
import Data.Set qualified as Set
import Hald.Config qualified as Config
import Hald.Deployment qualified as Dep
import Hald.Lock qualified as Lock
import Hald.Util (TreeAction (..), WalkStrategy (..))
import Hald.Util qualified as Util
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, removeDirectory, removeFile)
import System.FilePath ((</>))
import System.Posix.Files (deviceID, fileID, isRegularFile)
import UnliftIO.Async (pooledForConcurrently, pooledForConcurrentlyN_)
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

enableFsVerityOnCas :: Config.Config -> IO ()
enableFsVerityOnCas conf = do
  let casDir = Config.haldPath conf </> "objects"
  Util.walk (ParallelN 2) action casDir
  where
    action =
      TreeAction
        { dirAction = \_ _ -> pure (),
          symAction = \_ _ -> pure (),
          fileAction = \obj s ->
            when (isRegularFile s) $
              bracket_ (Lock.setMutable obj) (Lock.setImmutable obj) (Lock.enableFsVerity obj)
        }

collectGarbage :: Config.Config -> [Int] -> IO ()
collectGarbage conf keptDepIds = do
  threads <- getNumCapabilities
  let hp = Config.haldPath conf
      casDir = hp </> "objects"
      workThreads = max 1 $ div threads 2
  refSets <- pooledForConcurrently keptDepIds $ \depId -> do
    dep <- Dep.getDeployment depId conf
    case Dep.rootDir dep of
      Just root -> do
        localSetVar <- newTVarIO Set.empty
        Util.walk (ParallelN 2) (refAction localSetVar) (root </> "usr")
        readTVarIO localSetVar
      Nothing -> return Set.empty
  let refSet = Set.unions refSets

  dirExists <- doesDirectoryExist casDir
  when dirExists $ do
    prefixes <- listDirectory casDir
    pooledForConcurrentlyN_ 2 prefixes $ \p -> do
      let casPath = casDir </> p
      Lock.setMutable casPath
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
                ( \e -> do
                    let err = show (e :: IOException)
                    Util.printInfo err (Config.interactive conf)
                )
          Nothing -> return ()
  removeEmptyDirectories casDir
  where
    refAction var =
      TreeAction
        { dirAction = \_ _ -> return (),
          symAction = \_ _ -> return (),
          fileAction = \_ s ->
            when (isRegularFile s)
              $ atomically
              $ modifyTVar' var (Set.insert (deviceID s, fileID s))
        }

removeEmptyDirectories :: FilePath -> IO ()
removeEmptyDirectories = Util.walk (ParallelN 2) action
  where
    action =
      TreeAction
        { dirAction = \d _ -> do
            contents <- listDirectory d
            when (null contents)
              $ Util.ioOrPass
              $ removeDirectory d,
          symAction = \_ _ -> return (),
          fileAction = \_ _ -> return ()
        }
