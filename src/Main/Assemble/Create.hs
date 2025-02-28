module Main.Assemble.Create where

import Control.Monad (unless, when)
import Main.Assemble.Activate qualified as Asac
import Main.Assemble.Gc qualified as Asgc
import Main.Config qualified as Config
import Main.Container qualified as Container
import Main.Create qualified as Create
import Main.Deployment qualified as Dep
import Main.Lock qualified as Lock
import Main.Space qualified as Space
import Main.Util qualified as Util

deploymentCreationAssemblyPre :: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Config.Config -> IO ()
deploymentCreationAssemblyPre act build keep gc up se conf = do
  isRoot <- Util.rootCheck
  unless isRoot $ error "This action needs elevated privileges!"
  isLocked <- Util.acquireLock $ Config.configPath conf <> "/.hald.lock"
  unless isLocked $ error "Couldn't acquire lock!"
  deploymentCreationAssembly act build keep gc up se conf

deploymentCreationAssembly :: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Config.Config -> IO ()
deploymentCreationAssembly act build keep gc up se conf = do
  putStrLn "Initiating deployment creation..."
  updated <-
    if up
      then do
        putStrLn "Attempting to pull latest container image..."
        Container.pullImage (Config.containerUri conf)
      else return True
  existingDeps <- Dep.getDeploymentsInt (Config.haldPath conf) (Config.bootPath conf)
  let newDep = Dep.createDeployment existingDeps (Config.haldPath conf) (Config.bootPath conf)
  putStrLn $ "Creating Deployment " <> show (Dep.identifier newDep) <> "..."
  when updated $ do
    Lock.umountDirForcibly Lock.Simple $ Config.haldPath conf
    Space.gcBroken existingDeps (Config.rootDir conf) (Config.haldPath conf) (Config.bootPath conf)
    when build $
      Container.buildImage
        (Config.containerUri conf)
        (Config.localTag conf)
        (Config.configPath conf)
    let pbConf =
          if build
            then Config.applyConfigKey conf ["containerUri", Config.localTag conf]
            else conf
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
    when act $ Asac.deploymentActivationAssembly (Dep.identifier newDep) pbConf
    when gc $ Asgc.deploymentGcAssembly pbConf
    Lock.roBindMountDirToSelf Lock.Ro $ Config.haldPath conf
