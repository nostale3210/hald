module Main where

import Main.Assemble.Activate as Asac
import Main.Assemble.Create qualified as Ascr
import Main.Assemble.Gc qualified as Asgc
import Main.Assemble.Remove qualified as Asrm
import Main.Cli qualified as Cli
import Main.Config qualified as Config
import Main.Status qualified as Status
import Options.Applicative

main :: IO ()
main = assembleAction =<< execParser Cli.optsParser

assembleAction :: Cli.GlobalOpts -> IO ()
assembleAction parser = do
  case Cli.optCommand parser of
    Cli.Dep a b c d e f -> Ascr.deploymentCreationAssembly a b c d e f conf
    Cli.Rm x -> Asrm.deploymentErasureAssembly x conf
    Cli.Gc -> Asgc.deploymentGcAssembly conf
    Cli.Activate x -> Asac.deploymentActivationAssembly x conf
    Cli.Status -> Status.printDepStati conf
  where
    conf =
      if Cli.optRootdir parser == "/"
        then
          Config.updateContainerUri
            Config.defaultConfig
            (Config.containerImage Config.defaultConfig <> ":" <> Config.containerTag Config.defaultConfig)
        else
          Config.updateConfigPath
            ( Config.updateBootPath
                ( Config.updateHaldPath
                    ( Config.updateContainerUri
                        (Config.updateRootDir Config.defaultConfig $ Cli.optRootdir parser)
                        (Config.containerImage Config.defaultConfig <> ":" <> Config.containerTag Config.defaultConfig)
                    )
                    (Cli.optRootdir parser <> Config.haldPath Config.defaultConfig)
                )
                (Cli.optRootdir parser <> Config.bootPath Config.defaultConfig)
            )
            (Cli.optRootdir parser <> Config.configPath Config.defaultConfig)
