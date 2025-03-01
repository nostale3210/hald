module Main.Assemble.Create where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM qualified as Stm
import Control.Monad (unless, when)
import Data.ByteString.Char8 qualified as C
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
  msgChannel <- Stm.atomically Stm.newTChan
  let msgCont = Util.MessageContainer {Util.interactive = Config.interactive conf, Util.channel = msgChannel}
  _ <- forkIO (Util.printChannelMsg (Util.channel msgCont) $ C.pack "|/-\\")
  deploymentCreationAssembly act build keep gc up se conf msgCont

deploymentCreationAssembly :: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Config.Config -> Util.MessageContainer -> IO ()
deploymentCreationAssembly act build keep gc up se conf msgCont = do
  updated <-
    if up
      then do
        Util.printInfo "Attempting to pull latest container image..." (Util.interactive msgCont)
        Container.pullImage (Config.containerUri conf)
      else return True

  existingDeps <- Dep.getDeploymentsInt (Config.haldPath conf) (Config.bootPath conf)
  let newDep = Dep.createDeployment existingDeps (Config.haldPath conf) (Config.bootPath conf)

  when updated $ do
    Util.printInfo
      ("Creating Deployment " <> show (Dep.identifier newDep) <> "...")
      (Config.interactive conf)
    Lock.umountDirForcibly Lock.Simple $ Config.haldPath conf
    Space.gcBroken existingDeps conf

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

    Util.printProgress msgCont "Syncing container image to root..."
    Container.syncImage containerMount $ Config.haldPath pbConf
    Container.umountContainer "ald-root"

    Util.printProgress msgCont ("Syncing system config... (Dropping state: " <> show keep <> ")")
    Create.syncSystemConfig
      keep
      (Dep.identifier newDep)
      pbConf
    Create.createSkeleton (Dep.identifier newDep) pbConf

    Util.printProgress msgCont "Creating hardlinks to new deployment..."
    Create.hardlinkDep newDep (Config.haldPath pbConf)
    Container.rmContainer "ald-root"

    Util.printProgress msgCont "Placing kernel and initramfs..."
    Create.placeBootFiles newDep (Config.haldPath pbConf)
    Create.createBootEntry (Dep.identifier newDep) pbConf

    when se $ do
      Util.printProgress msgCont ("Relabeling deployment " <> show (Dep.identifier newDep) <> "...")
      Util.relabelSeLinuxPath
        (Config.haldPath pbConf <> "/" <> show (Dep.identifier newDep))
        ( Config.haldPath pbConf
            <> "/"
            <> show (Dep.identifier newDep)
            <> "/etc/selinux/targeted/contexts/files/file_contexts"
        )
        (Config.haldPath pbConf <> "/" <> show (Dep.identifier newDep))
        (Config.bootPath pbConf)

    when act $ Asac.deploymentActivationAssembly (Dep.identifier newDep) pbConf msgCont

    when gc $ Asgc.deploymentGcAssembly pbConf msgCont

    Lock.roBindMountDirToSelf Lock.Ro $ Config.haldPath pbConf
