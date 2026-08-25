module Hald.Cas.AssetMap
  ( TreeEntry (..),
    AssetMap,
    loadAssetMap,
    referencedObjects,
  )
where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet

data TreeEntry
  = TreeDir
  | TreeSymlink !B8.ByteString
  | TreeFile !B8.ByteString
  | TreeEmpty

type AssetMap = HashMap.HashMap B8.ByteString TreeEntry

loadAssetMap :: FilePath -> IO (Maybe AssetMap)
loadAssetMap path = do
  content <- BS.readFile path
  return $ HashMap.fromList <$> traverse parseEntry (B8.lines content)

parseEntry :: B8.ByteString -> Maybe (B8.ByteString, TreeEntry)
parseEntry line = case B8.split '\t' line of
  [tag, p] | tag == B8.singleton 'D' -> Just (p, TreeDir)
  [tag, p] | tag == B8.singleton 'E' -> Just (p, TreeEmpty)
  [tag, p, t] | tag == B8.singleton 'S' -> Just (p, TreeSymlink t)
  [tag, p, o] | tag == B8.singleton 'F' -> Just (p, TreeFile o)
  _ -> Nothing

referencedObjects :: AssetMap -> HashSet.HashSet B8.ByteString
referencedObjects = HashMap.foldl' addReference HashSet.empty
  where
    addReference refs entry = case entry of
      TreeFile casObject -> HashSet.insert casObject refs
      _ -> refs
