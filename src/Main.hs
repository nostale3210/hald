module Main where

import Control.Monad (when)
import Main.Activate qualified as Activate
import Main.Cli qualified as Cli
import Main.Config qualified as Config
import Main.Container qualified as Container
import Main.Create qualified as Create
import Main.Deployment qualified as Dep
import Main.Space qualified as Space
import Main.Util qualified as Util
import Options.Applicative

main :: IO ()
main = assembleAction =<< execParser Cli.optsParser

assembleAction :: Cli.GlobalOpts -> IO ()
assembleAction parser = do
  let config =
        if Cli.optRootdir parser == "/"
          then
            Config.updateContainerUri
              Config.defaultConfig
              (Config.containerImage Config.defaultConfig <> ":" <> Config.containerTag Config.defaultConfig)
          else
            Config.updateContainerUri
              (Config.updateRootDir Config.defaultConfig $ Cli.optRootdir parser)
              (Config.containerImage Config.defaultConfig <> ":" <> Config.containerTag Config.defaultConfig)
  case Cli.optCommand parser of
    Cli.Dep a b c d e f -> deploymentCreationAssembly a b c d e f config
    Cli.Rm x -> deploymentErasureAssembly x config
    Cli.Gc -> deploymentGcAssembly config
    Cli.Activate x -> deploymentActivationAssembly x config

deploymentCreationAssembly :: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Config.Config -> IO ()
deploymentCreationAssembly act build keep gc up se config = do
  putStrLn "Initiating creation of new deployment..."
  updated <-
    if up
      then do
        putStrLn "Attempting to pull latest container image..."
        Container.pullImage (Config.containerUri config)
      else return True
  existingDeps <- Dep.getDeploymentsInt (Config.haldPath config) (Config.bootPath config)
  let newDep = Dep.createDeployment existingDeps (Config.haldPath config) (Config.bootPath config)
  when updated $ do
    Space.gcBroken existingDeps (Config.rootDir config) (Config.haldPath config) (Config.bootPath config)
    when build $
      Container.buildImage
        (Config.containerUri config)
        (Config.localTag config)
        (Config.configPath config)
    let pbConf =
          if build
            then Config.updateContainerUri config $ Config.localTag config
            else config
    containerMount <- Container.mountContainer "ald-root" $ Config.containerUri pbConf
    Container.syncImage containerMount $ Config.haldPath pbConf
    Container.umountContainer "ald-root"
    Create.syncSystemConfig
      keep
      (Dep.identifier newDep)
      (Config.rootDir pbConf)
      (Config.haldPath pbConf)
      (Config.bootPath pbConf)
    Create.createSkeleton (Dep.identifier newDep) (Config.haldPath pbConf) (Config.bootPath pbConf)
    Create.hardlinkDep newDep (Config.haldPath pbConf)
    Container.rmContainer "ald-root"
    Create.placeBootFiles newDep (Config.haldPath pbConf)
    Create.createBootEntry (Dep.identifier newDep) (Config.rootDir pbConf) (Config.haldPath pbConf) (Config.bootPath pbConf)
    when se $
      Util.relabelSeLinuxPath
        (Config.haldPath pbConf <> "/" <> show (Dep.identifier newDep))
        ( Config.haldPath pbConf
            <> "/"
            <> show (Dep.identifier newDep)
            <> "/etc/selinux/targeted/contexts/files/file_contexts"
        )
        (Config.haldPath pbConf <> "/" <> show (Dep.identifier newDep))
        (Config.bootPath pbConf)
    when act $ deploymentActivationAssembly (Dep.identifier newDep) pbConf
    when gc $ deploymentGcAssembly pbConf

deploymentErasureAssembly :: Int -> Config.Config -> IO ()
deploymentErasureAssembly depId conf = do
  tbRmDep <- Dep.getDeployment depId (Config.rootDir conf) (Config.haldPath conf) (Config.bootPath conf)
  Space.rmDep tbRmDep (Config.rootDir conf) (Config.haldPath conf) (Config.bootPath conf)

deploymentGcAssembly :: Config.Config -> IO ()
deploymentGcAssembly conf = do
  putStrLn "Initiating garbage collection..."
  allDeps <- Dep.getDeploymentsInt (Config.haldPath conf) (Config.bootPath conf)
  Space.gcBroken allDeps (Config.rootDir conf) (Config.haldPath conf) (Config.bootPath conf)
  newAllDeps <- Dep.getDeploymentsInt (Config.haldPath conf) (Config.bootPath conf)
  Space.rmDeps
    (Config.keepDeps conf)
    newAllDeps
    (Config.rootDir conf)
    (Config.haldPath conf)
    (Config.bootPath conf)

deploymentActivationAssembly :: Int -> Config.Config -> IO ()
deploymentActivationAssembly newDepId conf = do
  putStrLn $ "Activating deployment " <> show newDepId <> "..."
  newDep <-
    Dep.getDeployment
      newDepId
      (Config.rootDir conf)
      (Config.haldPath conf)
      (Config.bootPath conf)
  Activate.activateNewRoot
    (Config.rootDir conf)
    newDep
    (Config.haldPath conf)
