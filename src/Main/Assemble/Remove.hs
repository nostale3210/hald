module Main.Assemble.Remove where

import Main.Config qualified as Config
import Main.Deployment qualified as Dep
import Main.Fail qualified as Fail
import Main.Lock qualified as Lock
import Main.Space qualified as Space
import Main.Util qualified as Util
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
  tbRmDep <- Dep.getDeployment depId (Config.rootDir conf) (Config.haldPath conf) (Config.bootPath conf)
  Fail.installGenericHandler [sigINT, sigTERM] conf (Just tbRmDep)

  Util.printInfo
    ("Removing deployment " <> show (Dep.identifier tbRmDep) <> "...")
    (Config.interactive conf)
  Util.printProgress msgCont ("Removing deployment " <> show (Dep.identifier tbRmDep) <> "...")
  Lock.umountDirForcibly Lock.Simple $ Config.haldPath conf
  Lock.umountDirForcibly Lock.Simple $ Config.haldPath conf <> "/" <> show (Dep.identifier tbRmDep) <> "/usr/local"
  Space.rmDep tbRmDep conf
  Lock.roBindMountDirToSelf Lock.Ro $ Config.haldPath conf
