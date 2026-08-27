module Hald.Assemble.Cas (gcAssembly, fsverityAssembly) where

import Hald.Cas.Gc qualified as CasGc
import Hald.Config qualified as Config
import Hald.Deployment qualified as Dep
import Hald.Lock qualified as Lock
import Hald.Mount qualified as Mount
import Hald.Util qualified as Util

gcAssembly :: Config.Config -> Util.MessageContainer -> IO ()
gcAssembly conf msgCont = do
  Util.printProgress msgCont "Performing CAS garbage collection..."

  allDeps <- Dep.getDeploymentsInt conf
  Lock.umountDirForcibly Lock.Simple $ Config.haldPath conf
  CasGc.collectGarbage conf allDeps
  CasGc.restoreStoreFlags conf
  Mount.roBindMountDirToSelf Mount.Ro $ Config.haldPath conf

fsverityAssembly :: Config.Config -> Util.MessageContainer -> IO ()
fsverityAssembly conf msgCont = do
  Util.printProgress msgCont "Enabling fs-verity on all CAS objects..."
  Lock.umountDirForcibly Lock.Simple $ Config.haldPath conf
  CasGc.enableFsVerityOnCas conf
  Mount.roBindMountDirToSelf Mount.Ro $ Config.haldPath conf
