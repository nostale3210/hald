module Main.Assemble.Remove where

import Control.Monad (unless)
import Main.Config qualified as Config
import Main.Deployment qualified as Dep
import Main.Lock qualified as Lock
import Main.Space qualified as Space
import Main.Util qualified as Util

deploymentErasureAssemblyPre :: Int -> Config.Config -> IO ()
deploymentErasureAssemblyPre depId conf = do
  isRoot <- Util.rootCheck
  unless isRoot $ error "This action needs elevated privileges!"
  isLocked <- Util.acquireLock $ Config.configPath conf <> "/.hald.lock"
  unless isLocked $ error "Couldn't acquire lock!"
  deploymentErasureAssembly depId conf

deploymentErasureAssembly :: Int -> Config.Config -> IO ()
deploymentErasureAssembly depId conf = do
  tbRmDep <- Dep.getDeployment depId (Config.rootDir conf) (Config.haldPath conf) (Config.bootPath conf)
  Lock.umountDirForcibly Lock.Simple $ Config.haldPath conf
  Space.rmDep tbRmDep (Config.rootDir conf) (Config.haldPath conf) (Config.bootPath conf)
  Lock.roBindMountDirToSelf Lock.Ro $ Config.haldPath conf
