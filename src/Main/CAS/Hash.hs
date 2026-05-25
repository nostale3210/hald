module Main.CAS.Hash (hashFile) where

import BLAKE3 qualified as B3
import Data.ByteString qualified as BS
import System.IO (Handle, IOMode (ReadMode), hClose, openFile)

chunkSize :: Int
chunkSize = 65536

hashFile :: FilePath -> IO String
hashFile path = do
  h <- openFile path ReadMode
  digest <- hReadHash h (B3.init Nothing)
  hClose h
  return $ show digest

hReadHash :: Handle -> B3.Hasher -> IO (B3.Digest B3.DEFAULT_DIGEST_LEN)
hReadHash h !hasher = do
  chunk <- BS.hGet h chunkSize
  if BS.null chunk
    then return (B3.finalize hasher)
    else hReadHash h (B3.update hasher [chunk])
