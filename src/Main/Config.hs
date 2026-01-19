module Main.Config where

import Control.Exception (IOException, catch)
import Main.Util qualified as Util

data PackageManager
  = Apk
  | Rpm
  | Unknown
  deriving (Show, Eq)

data Config
  = Config
  { haldPath :: FilePath,
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
    packageManager :: PackageManager
  }

defaultConfig :: Config
defaultConfig =
  Config
    { haldPath = "/.ald",
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
      packageManager = Unknown
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
applyConfigKeys conf confKeys =
  case confKeys of
    [] -> conf
    x : xs -> applyConfigKeys (applyConfigKey conf x) xs

applyConfigKey :: Config -> [String] -> Config
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
    "keepDeps" -> conf {keepDeps = read val :: Int}
    "rootDir" -> conf {rootDir = val}
    "interactive" -> conf {interactive = read val :: Bool}
    "packageManager" -> conf {packageManager = selectPm val}
    _ -> conf

selectPm :: String -> PackageManager
selectPm strMgr =
  case strMgr of
    "apk" -> Apk
    "rpm" -> Rpm
    _ -> Unknown
