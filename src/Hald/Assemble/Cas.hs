module Hald.Assemble.Cas (gcAssemblyPre) where

import Control.Exception (onException)
import Hald.Cas.Gc qualified as CasGc
import Hald.Config qualified as Config
import Hald.Deployment qualified as Dep
import Hald.Fail qualified as Fail
import Hald.Lock qualified as Lock
import Hald.Util qualified as Util
import System.Posix.Signals (sigINT, sigTERM)

gcAssemblyPre :: Config.Config -> Bool -> IO ()
gcAssemblyPre conf inhibit = do
  msgCont <- Util.genericRootfulPreproc (Config.configPath conf <> "/.hald.lock") (Config.interactive conf) inhibit
  Fail.installAsyncHandler [sigINT, sigTERM]
  flip onException (Fail.cleanupOnError conf Nothing (Just msgCont)) $
    gcAssembly conf msgCont

gcAssembly :: Config.Config -> Util.MessageContainer -> IO ()
gcAssembly conf msgCont = do
  Util.printProgress msgCont "Performing CAS garbage collection..."

  allDeps <- Dep.getDeploymentsInt conf
  Lock.umountDirForcibly Lock.Simple $ Config.haldPath conf
  CasGc.collectGarbage conf allDeps
  CasGc.restoreStoreFlags conf
  Lock.roBindMountDirToSelf Lock.Ro $ Config.haldPath conf
