module Main.Assemble.Remove (deploymentErasureAssembly) where

import Main.Config qualified as Config
import Main.Deployment qualified as Dep
import Main.Lock qualified as Lock
import Main.Space qualified as Space

deploymentErasureAssembly :: Int -> Config.Config -> IO ()
deploymentErasureAssembly depId conf = do
  tbRmDep <- Dep.getDeployment depId (Config.rootDir conf) (Config.haldPath conf) (Config.bootPath conf)
  Lock.umountDirForcibly Lock.Simple $ Config.haldPath conf
  Space.rmDep tbRmDep (Config.rootDir conf) (Config.haldPath conf) (Config.bootPath conf)
  Lock.roBindMountDirToSelf Lock.Ro $ Config.haldPath conf
