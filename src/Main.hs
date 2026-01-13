module Main where

import Main.Assemble.Activate as Asac
import Main.Assemble.Create qualified as Ascr
import Main.Assemble.Gc qualified as Asgc
import Main.Assemble.Remove qualified as Asrm
import Main.Cli qualified as Cli
import Main.Config qualified as Config
import Main.Diff qualified as Diff
import Main.Status qualified as Status
import Main.Util qualified as Util
import Options.Applicative

main :: IO ()
main = assembleAction =<< execParser Cli.optsParser

assembleAction :: Cli.GlobalOpts -> IO ()
assembleAction parser = do
  userConf <- Config.getUserConfig config
  interactive <- Util.checkInteractive
  let config' =
        if interactive
          then Config.applyConfigKey config ["interactive", show interactive]
          else config
      conf = Config.applyUserConfig config' userConf
  case Cli.optCommand parser of
    Cli.Dep a b c d e f g h -> Ascr.deploymentCreationAssemblyPre a b c d e f conf inhibit g h
    Cli.Rm x -> Asrm.deploymentErasureAssemblyPre x conf inhibit
    Cli.Gc -> Asgc.deploymentGcAssemblyPre conf inhibit
    Cli.Activate x -> Asac.deploymentActivationAssemblyPre x conf inhibit
    Cli.Status -> Status.printDepStati conf
    Cli.Diff x y -> Diff.printDiff x y "rpm" conf
  where
    inhibit = not $ Cli.optSystemd parser
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
