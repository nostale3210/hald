module Hald.Assemble.Activate where

import Control.Monad (unless)
import Hald.Activate qualified as Activate
import Hald.Assemble.Cas qualified as Ascas
import Hald.Config qualified as Config
import Hald.Deployment qualified as Dep
import Hald.Mount qualified as Mount
import Hald.Util qualified as Util

deploymentActivationAssembly :: Config.Config -> Util.MessageContainer -> Dep.Deployment -> IO ()
deploymentActivationAssembly conf msgCont newDep = do
  Util.printInfo ("Activating deployment " <> show (Dep.identifier newDep) <> "...") (Util.interactive msgCont)
  Util.printProgress msgCont ("Activating deployment " <> show (Dep.identifier newDep) <> "...")
  Ascas.verifyDigest conf newDep
  Activate.ensureOverlayEmptyDir (Config.haldPath conf)
  haldMounted <- Mount.isMountpoint (Config.haldPath conf)
  unless
    haldMounted
    (Mount.roBindMountDirToSelf Mount.Ro $ Config.haldPath conf)
  Activate.activateNewRoot
    (Config.rootDir conf)
    (Config.haldPath conf)
    newDep
  Mount.roRemountDir Mount.Rw $ root <> "etc"
  where
    root = Config.rootDir conf <> "/"
