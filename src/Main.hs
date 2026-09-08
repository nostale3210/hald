module Main where

import Hald.Assemble.Activate as Asac
import Hald.Assemble.Cas qualified as Ascas
import Hald.Assemble.Common qualified as Asm
import Hald.Assemble.Create qualified as Ascr
import Hald.Assemble.Gc qualified as Asgc
import Hald.Assemble.Remove qualified as Asrm
import Hald.Cli qualified as Cli
import Hald.Config qualified as Config
import Hald.Deployment qualified as Dep
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
      conf0 = Config.applyUserConfig config' userConf
  Util.setSystemThreads (Config.maxThreads conf0)
  case Cli.optCommand parser of
    Cli.Dep a b c d e f g h _cas j ->
      Asm.withRootfulAssembly
        conf0
        inhibit
        ( Dep.getDeploymentsInt conf0 >>= \existing ->
            return
              ( Just
                  (Dep.createDeployment existing conf0 (if j then Dep.Hardlink else Dep.Cas))
              )
        )
        (\msgCont newDep -> Ascr.deploymentCreationAssembly a b c d e f conf0 msgCont newDep g h j)
    Cli.Rm x ->
      Asm.withRootfulAssembly
        conf0
        inhibit
        (Just <$> Dep.getDeployment x conf0)
        (Asrm.deploymentErasureAssembly conf0)
    Cli.Gc ->
      Asm.withRootfulAssembly
        conf0
        inhibit
        (return Nothing)
        (\msgCont _dep -> Asgc.deploymentGcAssembly conf0 msgCont)
    Cli.Activate x mdigest ->
      Asm.withRootful
        conf0
        inhibit
        (Just <$> Dep.getDeployment x conf0)
        (Asac.deploymentActivationAssembly conf0 mdigest)
    Cli.Cas Cli.CasGc ->
      Asm.withRootfulAssembly
        conf0
        inhibit
        (return Nothing)
        (\msgCont _dep -> Ascas.gcAssembly conf0 msgCont)
    Cli.Cas Cli.CasFsverity ->
      Asm.withRootfulAssembly
        conf0
        inhibit
        (return Nothing)
        (\msgCont _dep -> Ascas.fsverityAssembly conf0 msgCont)
    Cli.Cas (Cli.CasVerify x) ->
      Asm.withRootful
        conf0
        inhibit
        (Just <$> Dep.getDeployment x conf0)
        (Ascas.verifyAssembly conf0)
    Cli.Status c -> Status.printDepStati conf0 c
    Cli.Diff x y -> Diff.printDiff x y conf0
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
