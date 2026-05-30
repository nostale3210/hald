module Hald.Assemble.Gc where

import Control.Exception (onException)
import Control.Monad (when)
import Data.List (sort)
import Hald.Cas.Gc qualified as CasGc
import Hald.Config qualified as Config
import Hald.Deployment qualified as Dep
import Hald.Fail qualified as Fail
import Hald.Lock qualified as Lock
import Hald.Space qualified as Space
import Hald.Util qualified as Util
import System.Posix.Signals (sigINT, sigTERM)

deploymentGcAssemblyPre :: Config.Config -> Bool -> IO ()
deploymentGcAssemblyPre conf inhibit = do
  msgCont <- Util.genericRootfulPreproc (Config.configPath conf <> "/.hald.lock") (Config.interactive conf) inhibit
  Fail.installAsyncHandler [sigINT, sigTERM]
  flip onException (Fail.cleanupOnError conf Nothing (Just msgCont)) $
    deploymentGcAssembly conf msgCont

deploymentGcAssembly :: Config.Config -> Util.MessageContainer -> IO ()
deploymentGcAssembly conf msgCont = do
  Util.printProgress msgCont "Performing garbage collection..."

  allDeps <- Dep.getDeploymentsInt conf
  Lock.umountDirForcibly Lock.Simple $ Config.haldPath conf
  Space.gcBroken allDeps conf
  newAllDeps <- Dep.getDeploymentsInt conf
  Space.rmDeps (Config.keepDeps conf) newAllDeps conf
  remainingDeps <- Dep.getDeploymentsInt conf
  when (sort allDeps /= sort remainingDeps) $
    CasGc.collectGarbage conf remainingDeps
  CasGc.restoreStoreFlags conf
  Lock.roBindMountDirToSelf Lock.Ro $ Config.haldPath conf
