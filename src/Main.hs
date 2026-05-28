module Main where

import Hald.Assemble.Activate as Asac
import Hald.Assemble.Create qualified as Ascr
import Hald.Assemble.Gc qualified as Asgc
import Hald.Assemble.Remove qualified as Asrm
import Hald.Cli qualified as Cli
import Hald.Config qualified as Config
import Hald.Diff qualified as Diff
import Hald.Status qualified as Status
import Hald.Util qualified as Util
import Options.Applicative (execParser)

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
  Util.setSystemThreads (Config.maxThreads conf)
  case Cli.optCommand parser of
    Cli.Dep a b c d e f g h i -> Ascr.deploymentCreationAssemblyPre a b c d e f conf inhibit g h i
    Cli.Rm x -> Asrm.deploymentErasureAssemblyPre x conf inhibit
    Cli.Gc -> Asgc.deploymentGcAssemblyPre conf inhibit
    Cli.Activate x -> Asac.deploymentActivationAssemblyPre x conf inhibit
    Cli.Status -> Status.printDepStati conf
    Cli.Diff x y -> Diff.printDiff x y conf
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
