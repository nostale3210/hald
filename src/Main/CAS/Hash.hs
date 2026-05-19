module Main.CAS.Hash (hashFile) where

import BLAKE3 qualified
import BLAKE3.IO qualified as BIO
import Data.ByteString qualified as BS

hashFile :: FilePath -> IO String
hashFile path = do
  contents <- BS.readFile path
  let digest = BLAKE3.hash Nothing [contents] :: BIO.Digest BIO.DEFAULT_DIGEST_LEN
  return $ show digest
