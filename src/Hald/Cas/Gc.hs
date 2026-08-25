module Hald.Cas.Gc (collectGarbage, restoreStoreFlags, enableFsVerityOnCas) where

import Control.Exception (IOException, bracket_, catch)
import Control.Monad (filterM, when)
import Data.ByteString.Char8 qualified as B8
import Data.HashSet qualified as HashSet
import Hald.Cas.AssetMap qualified as AssetMap
import Hald.Config qualified as Config
import Hald.Deployment qualified as Dep
import Hald.Lock qualified as Lock
import Hald.Space qualified as Space
import Hald.Util (TreeAction (..), WalkStrategy (..))
import Hald.Util qualified as Util
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, removeDirectory, removeFile)
import System.FilePath ((</>))
import System.Posix.Files (isRegularFile)
import UnliftIO.Async (pooledForConcurrently, pooledForConcurrentlyN_, pooledForConcurrently_)
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
  refSets <- pooledForConcurrently keptDepIds $ \depId ->
    (,) depId <$> depReferences conf depId
  let referenced = HashSet.unions [refs | (_, Just refs) <- refSets]
      brokenDepIds = [depId | (depId, Nothing) <- refSets]
  pooledForConcurrently_ brokenDepIds $ \depId -> do
    dep <- Dep.getDeployment depId conf
    Space.rmDep dep conf
  survivors <- filterM (stillPresent conf) brokenDepIds
  if null survivors
    then deleteUnreferencedObjects conf referenced
    else
      Util.printInfo
        ("Removing broken CAS deployments failed for:" <> show survivors <> " (GC skipped)")
        (Config.interactive conf)

depReferences :: Config.Config -> Int -> IO (Maybe (HashSet.HashSet B8.ByteString))
depReferences conf depId = do
  dep <- Dep.getDeployment depId conf
  case Dep.backend dep of
    Dep.Hardlink -> return (Just HashSet.empty)
    Dep.Cas -> maybe (return Nothing) referencesFromRoot (Dep.rootDir dep)
  where
    referencesFromRoot root = do
      hasAssetMap <- Util.pathExists (root </> "assetmap")
      if not hasAssetMap
        then return Nothing
        else do
          mAssetMap <- AssetMap.loadAssetMap (root </> "assetmap")
          return (AssetMap.referencedObjects <$> mAssetMap)

stillPresent :: Config.Config -> Int -> IO Bool
stillPresent conf depId =
  maybe False (const True) . Dep.rootDir <$> Dep.getDeployment depId conf

deleteUnreferencedObjects :: Config.Config -> HashSet.HashSet B8.ByteString -> IO ()
deleteUnreferencedObjects conf referenced = do
  threads <- getNumCapabilities
  let casDir = Config.haldPath conf </> "objects"
      workThreads = max 1 $ div threads 2
  dirExists <- doesDirectoryExist casDir
  when dirExists $ do
    prefixes <- listDirectory casDir
    pooledForConcurrentlyN_ 2 prefixes $ \p -> do
      let casPath = casDir </> p
      Lock.setMutable casPath
      objects <- listDirectory casPath
      pooledForConcurrentlyN_ workThreads objects $ \o -> do
        let casObj = casPath </> o
        when (not (HashSet.member (B8.pack (p </> o)) referenced)) $ do
          Lock.setMutable casObj
          catch
            (removeFile casObj)
            (\e -> Util.printInfo (show (e :: IOException)) (Config.interactive conf))
    removeEmptyDirectories casDir

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
