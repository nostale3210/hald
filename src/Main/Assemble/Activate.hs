module Main.Assemble.Activate where

import Control.Monad (unless)
import Main.Activate qualified as Activate
import Main.Config qualified as Config
import Main.Deployment qualified as Dep
import Main.Fail qualified as Fail
import Main.Lock qualified as Lock
import Main.Util qualified as Util
import System.Posix.Signals (sigINT, sigTERM)

deploymentActivationAssemblyPre :: Int -> Config.Config -> Bool -> IO ()
deploymentActivationAssemblyPre newDepId conf inhibit = do
  msgCont <- Util.genericRootfulPreproc (Config.configPath conf <> "/.hald.lock") (Config.interactive conf) inhibit
  Fail.installGenericHandler [sigINT, sigTERM] conf Nothing
  deploymentActivationAssembly newDepId conf msgCont

deploymentActivationAssembly :: Int -> Config.Config -> Util.MessageContainer -> IO ()
deploymentActivationAssembly newDepId conf msgCont = do
  Util.printInfo ("Activating deployment " <> show newDepId <> "...") (Util.interactive msgCont)
  Util.printProgress msgCont ("Activating deployment " <> show newDepId <> "...")
  haldMounted <- Util.isMountpoint (Config.haldPath conf)

  newDep <-
    Dep.getDeployment newDepId conf
  unless
    haldMounted
    (Lock.roBindMountDirToSelf Lock.Ro $ Config.haldPath conf)
  Activate.activateNewRoot
    (Config.rootDir conf)
    (Config.haldPath conf)
    newDep
  Lock.roRemountDir Lock.Rw $ root <> "etc"
  where
    root = Config.rootDir conf <> "/"
