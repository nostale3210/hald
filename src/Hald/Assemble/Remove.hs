module Hald.Assemble.Remove where

import Hald.Cas.Gc qualified as CasGc
import Hald.Config qualified as Config
import Hald.Deployment qualified as Dep
import Hald.Lock qualified as Lock
import Hald.Mount qualified as Mount
import Hald.Space qualified as Space
import Hald.Util qualified as Util

deploymentErasureAssembly :: Config.Config -> Util.MessageContainer -> Dep.Deployment -> IO ()
deploymentErasureAssembly conf msgCont dep = do
  Util.printInfo
    ("Removing deployment " <> show (Dep.identifier dep) <> "...")
    (Config.interactive conf)
  Util.printProgress msgCont ("Removing deployment " <> show (Dep.identifier dep) <> "...")
  Lock.umountDirForcibly Lock.Rfl $ Config.haldPath conf
  Space.rmDep dep conf
  CasGc.restoreStoreFlags conf
  Mount.roBindMountDirToSelf Mount.Ro $ Config.haldPath conf
