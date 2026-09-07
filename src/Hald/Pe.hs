module Hald.Pe (extractCmdline) where

import Control.Exception (IOException, try)
import Data.List (findIndex, isSubsequenceOf)
import System.Process (readProcess)

extractCmdline :: FilePath -> IO (Maybe String)
extractCmdline path = do
  result <- try @IOException $ readProcess "objdump" ["-s", "-j", ".cmdline", path] ""
  case result of
    Left _ -> return Nothing
    Right output -> return $ parseObjdumpOutput output

parseObjdumpOutput :: String -> Maybe String
parseObjdumpOutput output =
  case findIndex (isSubsequenceOf "Contents of section .cmdline") (lines output) of
    Nothing -> Nothing
    Just idx ->
      let dataLines = drop (idx + 1) (lines output)
          cmdline = reverse . dropWhile (`elem` ". ") . reverse . concatMap extractAsciiColumn $ dataLines
       in if null cmdline then Nothing else Just cmdline

extractAsciiColumn :: String -> String
extractAsciiColumn line =
  case findIndex (== ' ') (drop 1 line) of
    Nothing -> ""
    Just idx ->
      let asciiStart = idx + 1 + 38
       in if asciiStart < length line
            then drop asciiStart line
            else ""
