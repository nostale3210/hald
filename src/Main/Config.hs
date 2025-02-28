module Main.Config where

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

defaultConfig :: Config
defaultConfig =
  Config
    { haldPath = "/.ald",
      bootPath = "/boot",
      configPath = "/etc/hald",
      containerImage = "ghcr.io/nostale3210/timesinkc-cosmic-nvidia-hald",
      containerTag = "latest",
      containerUri = "",
      localTag = "localhost/ald-custom",
      keepDeps = 4,
      rootDir = ""
    }

updateHaldPath :: Config -> FilePath -> Config
updateHaldPath conf fp = conf {haldPath = fp}

updateBootPath :: Config -> FilePath -> Config
updateBootPath conf fp = conf {bootPath = fp}

updateConfigPath :: Config -> FilePath -> Config
updateConfigPath conf fp = conf {configPath = fp}

updateContainerImage :: Config -> String -> Config
updateContainerImage conf im = conf {containerImage = im}

updateContainerTag :: Config -> String -> Config
updateContainerTag conf tag = conf {containerTag = tag}

updateContainerUri :: Config -> String -> Config
updateContainerUri conf uri = conf {containerUri = uri}

updateLocalTag :: Config -> String -> Config
updateLocalTag conf tag = conf {localTag = tag}

updateKeepDeps :: Config -> Int -> Config
updateKeepDeps conf deps = conf {keepDeps = deps}

updateRootDir :: Config -> FilePath -> Config
updateRootDir conf dir = conf {rootDir = dir}
