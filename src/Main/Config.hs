module Main.Config where

import Control.Exception (IOException, catch)
import Main.Util qualified as Util

data Config
  = Config
  { haldPath :: FilePath,
    bootPath :: FilePath,
    configPath :: FilePath,
    containerImage :: String,
    containerTag :: String,
    containerUri :: String,
    localTag :: String,
    keepDeps :: Int,
    rootDir :: FilePath
  }
  deriving (Show)

defaultConfig :: Config
defaultConfig =
  Config
    { haldPath = "/.ald",
      bootPath = "/boot",
      configPath = "/etc/hald",
      containerImage = "ghcr.io/nostale3210/timesinkc-main-hald",
      containerTag = "latest",
      containerUri = "",
      localTag = "localhost/hald-custom",
      keepDeps = 4,
      rootDir = ""
    }

getUserConfig :: Config -> IO String
getUserConfig conf = do
  configExists <- Util.pathExists $ configPath conf <> "/hald.conf"
  if configExists
    then
      catch
        (readFile (configPath conf <> "/hald.conf"))
        ( \e -> do
            let err = show (e :: IOException)
            putStrLn $ "Couldn't read config file; " <> err
            return ""
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
    _ -> conf
