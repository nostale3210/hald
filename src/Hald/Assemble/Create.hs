module Hald.Assemble.Create where

import Control.Monad (unless, when)
import Data.Maybe (isNothing)
import Hald.Assemble.Activate qualified as Asac
import Hald.Assemble.Common qualified as Asm
import Hald.Assemble.Gc qualified as Asgc
import Hald.Cas.Gc qualified as CasGc
import Hald.Cas.Verify qualified as CasVer
import Hald.Config qualified as Config
import Hald.Container qualified as Container
import Hald.Create qualified as Create
import Hald.Deployment qualified as Dep
import Hald.Lock qualified as Lock
import Hald.Space qualified as Space
import Hald.Util qualified as Util
import System.FilePath ((</>))
import System.Mem (performGC)

deploymentCreationAssembly :: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Config.Config -> Util.MessageContainer -> Dep.Deployment -> Bool -> Bool -> Bool -> IO ()
deploymentCreationAssembly act build keep gc up se conf msgCont newDep sb uki hardlink = do
  updated <-
    if up
      then do
        Util.printInfo "Attempting to pull latest container image..." (Util.interactive msgCont)
        Container.pullImage conf
      else return True

  existingDeps <- Dep.getDeploymentsInt conf
  let backend = if hardlink then Dep.Hardlink else Dep.Cas

  when updated $ Asm.withHaldStoreUnmounted conf Lock.Simple $ do
    Util.printInfo
      ("Creating Deployment " <> show (Dep.identifier newDep) <> "...")
      (Config.interactive conf)
    Space.gcBroken existingDeps conf
    remainingDeps <- Dep.getDeploymentsInt conf
    let linkSource = case filter (< Dep.identifier newDep) remainingDeps of
          [] -> Nothing
          xs -> Just $ maximum xs

    when build $ Container.buildImage conf
    let pbConf =
          if build
            then Config.applyConfigKey conf ["containerUri", Config.localTag conf]
            else conf
    containerMount <- Container.mountContainer "hald-root" $ Config.containerUri pbConf

    Create.createSkeleton (Dep.identifier newDep) pbConf uki backend

    Util.printProgress msgCont "Syncing deployment usr..."
    layerDiffs <- Container.getLayerInfo "hald-root"
    Create.syncDeploymentUsr containerMount pbConf newDep linkSource layerDiffs

    Util.printProgress msgCont "Syncing deployment etc..."
    Create.syncDeploymentEtc containerMount pbConf newDep

    Util.printProgress msgCont "Normalizing container etc timestamps..."
    Create.normalizeDepEtcTimestamps pbConf newDep

    Util.printProgress msgCont ("Syncing system config... (Dropping state: " <> show keep <> ")")
    Create.syncSystemConfig keep pbConf newDep

    unless (isNothing (Config.packageDB pbConf)) $
      Create.getPackageDB containerMount pbConf newDep
    Container.umountContainer "hald-root"

    Util.printProgress msgCont "Writing lockfile..."
    Create.writeLockfile pbConf newDep
    Container.rmContainer "hald-root"

    Util.printProgress msgCont "Placing kernel and initramfs..."
    if uki
      then do
        digest <- CasVer.getDeploymentDigest pbConf newDep
        Create.installUki pbConf newDep digest
      else
        Create.placeBootFiles pbConf newDep
          >> Create.createBootEntry (Dep.identifier newDep) pbConf

    when se $ do
      Util.printProgress msgCont ("Relabeling deployment " <> show (Dep.identifier newDep) <> "...")
      when (backend == Dep.Cas)
        $ Lock.clearRecursiveImmutable
        $ Dep.rootDirFor pbConf (Dep.identifier newDep) </> "usr"
      Util.relabelSeLinuxPath
        (Dep.rootDirFor pbConf (Dep.identifier newDep))
        "/etc/selinux/targeted/contexts/files/file_contexts"
        (Config.bootPath pbConf)
      when (backend == Dep.Cas) $ do
        Lock.setImmutable $ Dep.rootDirFor pbConf (Dep.identifier newDep) </> "empty"
        Lock.setImmutable $ Dep.rootDirFor pbConf (Dep.identifier newDep) </> "usr/.hald_dep"

    when sb $ do
      Util.printProgress msgCont ("Signing deployment " <> show (Dep.identifier newDep) <> " kernel...")
      signingSuccess <-
        if uki
          then
            Util.signKernel (Config.ukiPath pbConf) (Dep.identifier newDep) ".efi"
          else
            Util.signKernel (Config.bootPath pbConf) (Dep.identifier newDep) "/vmlinuz"
      unless signingSuccess (Util.printInfo "Signing kernel failed!" (Config.interactive pbConf))

    Util.printProgress msgCont "Setting default bootloader entry..."
    Create.setDefaultBootEntry (Dep.identifier newDep)

    when act $ Asac.deploymentActivationAssembly pbConf Nothing msgCont newDep

    when gc $ performGC >> Asgc.deploymentGcAssembly pbConf msgCont

    unless gc $ CasGc.restoreStoreFlags pbConf
