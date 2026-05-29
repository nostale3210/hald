module Hald.Assemble.Create where

import Control.Exception (onException)
import Control.Monad (unless, when)
import Data.Maybe (isNothing)
import Hald.Assemble.Activate qualified as Asac
import Hald.Assemble.Gc qualified as Asgc
import Hald.Cas.Gc qualified as CasGc
import Hald.Config qualified as Config
import Hald.Container qualified as Container
import Hald.Create qualified as Create
import Hald.Deployment qualified as Dep
import Hald.Fail qualified as Fail
import Hald.Lock qualified as Lock
import Hald.Space qualified as Space
import Hald.Util qualified as Util
import System.FilePath ((</>))
import System.Mem (performGC)
import System.Posix.Signals (sigINT, sigTERM)

deploymentCreationAssemblyPre :: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Config.Config -> Bool -> Bool -> Bool -> Bool -> IO ()
deploymentCreationAssemblyPre act build keep gc up se conf inhibit sb uki cas = do
  msgCont <- Util.genericRootfulPreproc (Config.configPath conf <> "/.hald.lock") (Config.interactive conf) inhibit
  deploymentCreationAssembly act build keep gc up se conf msgCont sb uki cas

deploymentCreationAssembly :: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Config.Config -> Util.MessageContainer -> Bool -> Bool -> Bool -> IO ()
deploymentCreationAssembly act build keep gc up se conf msgCont sb uki cas = do
  updated <-
    if up
      then do
        Util.printInfo "Attempting to pull latest container image..." (Util.interactive msgCont)
        Container.pullImage conf
      else return True

  existingDeps <- Dep.getDeploymentsInt conf
  let backend = if cas then Dep.Cas else Dep.Hardlink
      newDep = Dep.createDeployment existingDeps conf backend

  Fail.installAsyncHandler [sigINT, sigTERM]
  flip onException (Fail.cleanupOnError conf (Just newDep) (Just msgCont)) $
    when updated $ do
      Util.printInfo
        ("Creating Deployment " <> show (Dep.identifier newDep) <> "...")
        (Config.interactive conf)
      Lock.umountDirForcibly Lock.Simple $ Config.haldPath conf
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
      Create.syncDeploymentUsr containerMount pbConf newDep linkSource

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
        then Create.installUki pbConf newDep
        else
          Create.placeBootFiles pbConf newDep
            >> Create.createBootEntry (Dep.identifier newDep) pbConf

      when se $ do
        Util.printProgress msgCont ("Relabeling deployment " <> show (Dep.identifier newDep) <> "...")
        when cas $
          Lock.clearRecursiveImmutable $
            Config.haldPath pbConf </> show (Dep.identifier newDep) </> "usr"
        Util.relabelSeLinuxPath
          (Config.haldPath pbConf </> show (Dep.identifier newDep))
          "/etc/selinux/targeted/contexts/files/file_contexts"
          (Config.bootPath pbConf)

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

      when act $ Asac.deploymentActivationAssembly (Dep.identifier newDep) pbConf msgCont

      when gc $ performGC >> Asgc.deploymentGcAssembly pbConf msgCont

      unless gc $ CasGc.restoreStoreFlags pbConf
      Lock.roBindMountDirToSelf Lock.Ro $ Config.haldPath pbConf
