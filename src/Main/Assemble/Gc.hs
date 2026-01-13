module Main.Assemble.Gc where

import Main.Config qualified as Config
import Main.Deployment qualified as Dep
import Main.Fail qualified as Fail
import Main.Lock qualified as Lock
import Main.Space qualified as Space
import Main.Util qualified as Util
import System.Posix.Signals (sigINT, sigTERM)

deploymentGcAssemblyPre :: Config.Config -> Bool -> IO ()
deploymentGcAssemblyPre conf inhibit = do
  msgCont <- Util.genericRootfulPreproc (Config.configPath conf <> "/.hald.lock") (Config.interactive conf) inhibit
  Fail.installGenericHandler [sigINT, sigTERM] conf Nothing
  deploymentGcAssembly conf msgCont

deploymentGcAssembly :: Config.Config -> Util.MessageContainer -> IO ()
deploymentGcAssembly conf msgCont = do
  Util.printProgress msgCont "Performing garbage collection..."

  allDeps <- Dep.getDeploymentsInt conf
  Lock.umountDirForcibly Lock.Simple $ Config.haldPath conf
  Space.gcBroken allDeps conf
  newAllDeps <- Dep.getDeploymentsInt conf
  Space.rmDeps (Config.keepDeps conf) newAllDeps conf
  Lock.roBindMountDirToSelf Lock.Ro $ Config.haldPath conf
