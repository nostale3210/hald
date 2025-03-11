module Main.Assemble.Activate where

import Main.Activate qualified as Activate
import Main.Config qualified as Config
import Main.Deployment qualified as Dep
import Main.Fail qualified as Fail
import Main.Lock qualified as Lock
import Main.Util qualified as Util
import System.Posix.Signals (sigINT, sigTERM)

deploymentActivationAssemblyPre :: Int -> Config.Config -> IO ()
deploymentActivationAssemblyPre newDepId conf = do
  msgCont <- Util.genericRootfulPreproc (Config.configPath conf <> "/.hald.lock") (Config.interactive conf)
  Fail.installGenericHandler [sigINT, sigTERM] conf Nothing
  deploymentActivationAssembly newDepId conf msgCont

deploymentActivationAssembly :: Int -> Config.Config -> Util.MessageContainer -> IO ()
deploymentActivationAssembly newDepId conf msgCont = do
  Util.printInfo ("Activating deployment " <> show newDepId <> "...") (Util.interactive msgCont)
  Util.printProgress msgCont ("Activating deployment " <> show newDepId <> "...")

  newDep <-
    Dep.getDeployment
      newDepId
      (Config.rootDir conf)
      (Config.haldPath conf)
      (Config.bootPath conf)
  let root = Config.rootDir conf <> "/"
  Lock.unlockRoot root
  Lock.umountDirForcibly Lock.Simple $ Config.haldPath conf
  Lock.umountDirForcibly Lock.Rfl $ root <> "usr"
  Activate.activateNewRoot
    (Config.rootDir conf)
    newDep
    (Config.haldPath conf)
  Lock.roBindMountDirToSelf Lock.Ro $ root <> "usr"
  Lock.roBindMountDirToSelf Lock.Rw $ root <> "usr/local"
  Lock.roBindMountDirToSelf Lock.Ro $ Config.haldPath conf
  Lock.lockRoot root
