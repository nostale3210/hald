module Main.Assemble.Create where

import Control.Monad (when)
import Main.Assemble.Activate qualified as Asac
import Main.Assemble.Gc qualified as Asgc
import Main.Config qualified as Config
import Main.Container qualified as Container
import Main.Create qualified as Create
import Main.Deployment qualified as Dep
import Main.Fail qualified as Fail
import Main.Lock qualified as Lock
import Main.Space qualified as Space
import Main.Util qualified as Util
import System.Posix.Signals (sigINT, sigTERM)

deploymentCreationAssemblyPre :: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Config.Config -> Bool -> IO ()
deploymentCreationAssemblyPre act build keep gc up se conf inhibit = do
  msgCont <- Util.genericRootfulPreproc (Config.configPath conf <> "/.hald.lock") (Config.interactive conf) inhibit
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

  Fail.installGenericHandler [sigINT, sigTERM] conf (Just newDep)

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
    Create.syncSystemConfig keep pbConf
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
