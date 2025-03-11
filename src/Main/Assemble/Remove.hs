module Main.Assemble.Remove where

import Main.Config qualified as Config
import Main.Deployment qualified as Dep
import Main.Fail qualified as Fail
import Main.Lock qualified as Lock
import Main.Space qualified as Space
import Main.Util qualified as Util
import System.Posix.Signals (sigINT, sigTERM)

deploymentErasureAssemblyPre :: Int -> Config.Config -> IO ()
deploymentErasureAssemblyPre depId conf = do
  _ <- Util.genericRootfulPreproc (Config.configPath conf <> "/.hald.lock") (Config.interactive conf)
  deploymentErasureAssembly depId conf

deploymentErasureAssembly :: Int -> Config.Config -> IO ()
deploymentErasureAssembly depId conf = do
  tbRmDep <- Dep.getDeployment depId (Config.rootDir conf) (Config.haldPath conf) (Config.bootPath conf)
  Fail.installGenericHandler [sigINT, sigTERM] conf (Just tbRmDep)

  Lock.umountDirForcibly Lock.Simple $ Config.haldPath conf
  Space.rmDep tbRmDep conf
  Lock.roBindMountDirToSelf Lock.Ro $ Config.haldPath conf
