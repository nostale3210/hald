module Main.Config where
haldPath :: FilePath
haldPath = "dummy/.ald"

bootPath :: FilePath
bootPath = "dummy/boot"

configPath :: FilePath
configPath = "/etc/hald"

containerImage :: String
containerImage = "ghcr.io/nostale3210/timesinkc-cosmic-nvidia-ald"

containerTag :: String
containerTag = "latest"

containerUri :: String
containerUri = containerImage <> ":" <> containerTag

localTag :: String
localTag = "localhost/ald-custom"

discardState :: Bool
discardState = False
