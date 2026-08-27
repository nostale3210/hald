module Hald.Assemble.Gc where

import Control.Monad (when)
import Data.List (sort)
import Hald.Assemble.Common qualified as Asm
import Hald.Cas.Gc qualified as CasGc
import Hald.Config qualified as Config
import Hald.Deployment qualified as Dep
import Hald.Lock qualified as Lock
import Hald.Space qualified as Space
import Hald.Util qualified as Util

deploymentGcAssembly :: Config.Config -> Util.MessageContainer -> IO ()
deploymentGcAssembly conf msgCont = do
  Util.printProgress msgCont "Performing garbage collection..."

  allDeps <- Dep.getDeploymentsInt conf
  Asm.withHaldStoreUnmounted conf Lock.Simple $ do
    Space.gcBroken allDeps conf
    newAllDeps <- Dep.getDeploymentsInt conf
    Space.rmDeps (Config.keepDeps conf) newAllDeps conf
    remainingDeps <- Dep.getDeploymentsInt conf
    when (sort allDeps /= sort remainingDeps) $
      CasGc.collectGarbage conf remainingDeps
    CasGc.restoreStoreFlags conf
