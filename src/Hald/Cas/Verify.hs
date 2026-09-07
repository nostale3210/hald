module Hald.Cas.Verify (getDeploymentDigest) where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.HashSet qualified as HashSet
import Data.List (sortOn)
import Hald.Cas.AssetMap qualified as AssetMap
import Hald.Config qualified as Config
import Hald.Deployment qualified as Dep
import Hald.Lock qualified as Lock
import Hald.Util qualified as Util
import System.FilePath ((</>))
import UnliftIO.Async (pooledMapConcurrently)

getDeploymentDigest :: Config.Config -> Int -> IO (Maybe BS.ByteString)
getDeploymentDigest conf depId = do
  dep <- Dep.getDeployment depId conf
  case Dep.backend dep of
    Dep.Hardlink -> return $ Just BS.empty
    Dep.Cas -> case Dep.rootDir dep of
      Nothing -> return Nothing
      Just root ->
        AssetMap.loadAssetMap (root </> "assetmap") >>= \maybeAM ->
          case maybeAM of
            Nothing -> return Nothing
            Just am -> do
              let objects = sortOn B8.unpack $ HashSet.toList (AssetMap.referencedObjects am)
              results <- pooledMapConcurrently verifyObject objects
              return $ fmap BS.concat . sequence $ results
  where
    verifyObject obj = do
      let path = Config.haldPath conf </> "objects" </> B8.unpack obj
      mDigest <- Lock.measureFsVerity path
      case mDigest of
        Nothing -> do
          Util.printInfo ("File lacks fsverity: " <> path) (Config.interactive conf)
          return Nothing
        Just digest -> return $ Just digest
