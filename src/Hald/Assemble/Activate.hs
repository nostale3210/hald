module Hald.Assemble.Activate where

import Control.Exception (onException)
import Control.Monad (unless)
import Hald.Activate qualified as Activate
import Hald.Config qualified as Config
import Hald.Deployment qualified as Dep
import Hald.Fail qualified as Fail
import Hald.Lock qualified as Lock
import Hald.Util qualified as Util
import System.Posix.Signals (sigINT, sigTERM)

deploymentActivationAssemblyPre :: Int -> Config.Config -> Bool -> IO ()
deploymentActivationAssemblyPre newDepId conf inhibit = do
  msgCont <- Util.genericRootfulPreproc (Config.configPath conf <> "/.hald.lock") (Config.interactive conf) inhibit
  Fail.installAsyncHandler [sigINT, sigTERM]
  flip onException (Fail.cleanupOnError conf Nothing (Just msgCont)) $
    deploymentActivationAssembly newDepId conf msgCont

deploymentActivationAssembly :: Int -> Config.Config -> Util.MessageContainer -> IO ()
deploymentActivationAssembly newDepId conf msgCont = do
  Util.printInfo ("Activating deployment " <> show newDepId <> "...") (Util.interactive msgCont)
  Util.printProgress msgCont ("Activating deployment " <> show newDepId <> "...")
  Activate.ensureOverlayEmptyDir (Config.haldPath conf)
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
