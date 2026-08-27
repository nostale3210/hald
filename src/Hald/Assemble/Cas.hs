module Hald.Assemble.Cas (gcAssembly, fsverityAssembly) where

import Hald.Assemble.Common qualified as Asm
import Hald.Cas.Gc qualified as CasGc
import Hald.Config qualified as Config
import Hald.Deployment qualified as Dep
import Hald.Lock qualified as Lock
import Hald.Util qualified as Util

gcAssembly :: Config.Config -> Util.MessageContainer -> IO ()
gcAssembly conf msgCont = do
  Util.printProgress msgCont "Performing CAS garbage collection..."
  allDeps <- Dep.getDeploymentsInt conf
  Asm.withHaldStoreUnmounted conf Lock.Simple $ do
    CasGc.collectGarbage conf allDeps
    CasGc.restoreStoreFlags conf

fsverityAssembly :: Config.Config -> Util.MessageContainer -> IO ()
fsverityAssembly conf msgCont = do
  Util.printProgress msgCont "Enabling fs-verity on all CAS objects..."
  Asm.withHaldStoreUnmounted conf Lock.Simple $
    CasGc.enableFsVerityOnCas conf
