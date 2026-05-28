module Hald.Assemble.Remove where

import Control.Exception (onException)
import Hald.Cas.Gc qualified as CasGc
import Hald.Config qualified as Config
import Hald.Deployment qualified as Dep
import Hald.Fail qualified as Fail
import Hald.Lock qualified as Lock
import Hald.Space qualified as Space
import Hald.Util qualified as Util
import System.Posix.Signals (sigINT, sigTERM)

deploymentErasureAssemblyPre :: Int -> Config.Config -> Bool -> IO ()
deploymentErasureAssemblyPre depId conf inhibit =
  Util.genericRootfulPreproc
    (Config.configPath conf <> "/.hald.lock")
    (Config.interactive conf)
    inhibit
    >>= \msgContent ->
      deploymentErasureAssembly depId conf msgContent

deploymentErasureAssembly :: Int -> Config.Config -> Util.MessageContainer -> IO ()
deploymentErasureAssembly depId conf msgCont = do
  tbRmDep <- Dep.getDeployment depId conf
  Fail.installAsyncHandler [sigINT, sigTERM]
  flip onException (Fail.cleanupOnError conf (Just tbRmDep) (Just msgCont)) $ do
    Util.printInfo
      ("Removing deployment " <> show (Dep.identifier tbRmDep) <> "...")
      (Config.interactive conf)
    Util.printProgress msgCont ("Removing deployment " <> show (Dep.identifier tbRmDep) <> "...")
    Lock.umountDirForcibly Lock.Rfl $ Config.haldPath conf
    Space.rmDep tbRmDep conf
    CasGc.restoreStoreFlags conf
    Lock.roBindMountDirToSelf Lock.Ro $ Config.haldPath conf
