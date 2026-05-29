module Hald.Config where

import Control.Exception (IOException, catch)
import Data.List (foldl')
import Hald.Util qualified as Util

data PackageManager
  = Apk
  | Pacman
  | Rpm
  | Xbps
  | Unknown
  deriving (Show, Eq)

newtype LegacyPaths = LegacyPaths {legacyPath :: Maybe FilePath}
  deriving (Show, Eq)

noLegacyPaths :: LegacyPaths
noLegacyPaths = LegacyPaths Nothing

data Config
  = Config
  { haldPath :: FilePath,
    legacyPaths :: LegacyPaths,
    bootPath :: FilePath,
    ukiPath :: FilePath,
    configPath :: FilePath,
    containerImage :: String,
    containerTag :: String,
    containerUri :: String,
    localTag :: String,
    keepDeps :: Int,
    rootDir :: FilePath,
    interactive :: Bool,
    packageManager :: PackageManager,
    packageDB :: Maybe FilePath,
    maxThreads :: Int
  }

defaultConfig :: Config
defaultConfig =
  Config
    { haldPath = "/.hald",
      legacyPaths = noLegacyPaths,
      bootPath = "/boot",
      ukiPath = bootPath defaultConfig <> "/EFI/Linux",
      configPath = "/etc/hald",
      containerImage = "ghcr.io/nostale3210/timesinkc-main-hald",
      containerTag = "latest",
      containerUri = "",
      localTag = "localhost/hald-custom",
      keepDeps = 4,
      rootDir = "",
      interactive = False,
      packageManager = Unknown,
      packageDB = Nothing,
      maxThreads = 9999
    }

getUserConfig :: Config -> IO String
getUserConfig conf = do
  configExists <- Util.pathExists $ configPath conf <> "/hald.conf"
  if configExists
    then
      catch
        (readFile (configPath conf <> "/hald.conf"))
        ( \e ->
            let err = show (e :: IOException)
             in putStrLn ("Couldn't read config file; " <> err)
                  >> return ""
        )
    else return ""

applyUserConfig :: Config -> String -> Config
applyUserConfig conf userConf =
  let confLines = lines userConf
      confKeys = map words confLines
   in applyConfigKeys conf confKeys

applyConfigKeys :: Config -> [[String]] -> Config
applyConfigKeys = foldl' applyConfigKey

applyConfigKey :: Config -> [String] -> Config
applyConfigKey conf [] = conf
applyConfigKey conf configKey = updateSingleKey conf (head configKey) (last configKey)

updateSingleKey :: Config -> String -> String -> Config
updateSingleKey conf key val =
  case key of
    "haldPath" -> conf {haldPath = val}
    "bootPath" -> conf {bootPath = val}
    "ukiPath" -> conf {ukiPath = val}
    "configPath" -> conf {configPath = val}
    "containerImage" ->
      applyConfigKey
        (conf {containerImage = val})
        [ "containerUri",
          val
            <> ":"
            <> containerTag conf
        ]
    "containerTag" ->
      applyConfigKey
        (conf {containerTag = val})
        [ "containerUri",
          containerImage conf
            <> ":"
            <> val
        ]
    "containerUri" -> conf {containerUri = val}
    "localTag" -> conf {localTag = val}
    "keepDeps" -> conf {keepDeps = readVal (keepDeps conf) val}
    "rootDir" -> conf {rootDir = val}
    "interactive" -> conf {interactive = readVal (interactive conf) val}
    "packageManager" -> conf {packageManager = selectPm val}
    "packageDB" -> conf {packageDB = Just val}
    "maxThreads" -> conf {maxThreads = readVal (maxThreads conf) val}
    _ -> conf

readVal :: (Read a) => a -> String -> a
readVal def s = case reads s of [(v, "")] -> v; _ -> def

selectPm :: String -> PackageManager
selectPm strMgr =
  case strMgr of
    "apk" -> Apk
    "pacman" -> Pacman
    "rpm" -> Rpm
    "xbps" -> Xbps
    _ -> Unknown
