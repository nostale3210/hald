module Main where

import Main.Assemble.Activate as Asac
import Main.Assemble.Create qualified as Ascr
import Main.Assemble.Gc qualified as Asgc
import Main.Assemble.Remove qualified as Asrm
import Main.Cli qualified as Cli
import Main.Config qualified as Config
import Main.Diff qualified as Diff
import Main.Status qualified as Status
import Options.Applicative

main :: IO ()
main = assembleAction =<< execParser Cli.optsParser

assembleAction :: Cli.GlobalOpts -> IO ()
assembleAction parser = do
  userConf <- Config.getUserConfig config
  let conf = Config.applyUserConfig config userConf
  case Cli.optCommand parser of
    Cli.Dep a b c d e f -> Ascr.deploymentCreationAssembly a b c d e f conf
    Cli.Rm x -> Asrm.deploymentErasureAssembly x conf
    Cli.Gc -> Asgc.deploymentGcAssembly conf
    Cli.Activate x -> Asac.deploymentActivationAssembly x conf
    Cli.Status -> Status.printDepStati conf
    Cli.Diff x y -> Diff.printDiff x y "rpm" conf
  where
    config =
      if Cli.optRootdir parser == "/"
        then
          Config.applyConfigKey
            Config.defaultConfig
            [ "containerUri",
              Config.containerImage Config.defaultConfig
                <> ":"
                <> Config.containerTag Config.defaultConfig
            ]
        else
          Config.applyConfigKeys
            Config.defaultConfig
            [ ["haldPath", Cli.optRootdir parser <> Config.haldPath Config.defaultConfig],
              ["bootPath", Cli.optRootdir parser <> Config.bootPath Config.defaultConfig],
              ["configPath", Cli.optRootdir parser <> Config.configPath Config.defaultConfig],
              [ "containerUri",
                Config.containerImage Config.defaultConfig
                  <> ":"
                  <> Config.containerTag Config.defaultConfig
              ],
              ["rootDir", Cli.optRootdir parser]
            ]
