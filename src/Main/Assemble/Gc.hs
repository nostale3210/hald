module Main.Assemble.Gc where

import Control.Monad (unless)
import Main.Config qualified as Config
import Main.Deployment qualified as Dep
import Main.Lock qualified as Lock
import Main.Space qualified as Space
import Main.Util qualified as Util

deploymentGcAssemblyPre :: Config.Config -> IO ()
deploymentGcAssemblyPre conf = do
  isRoot <- Util.rootCheck
  unless isRoot $ error "This action needs elevated privileges!"
  isLocked <- Util.acquireLock $ Config.configPath conf <> "/.hald.lock"
  unless isLocked $ error "Couldn't acquire lock!"
  deploymentGcAssembly conf

deploymentGcAssembly :: Config.Config -> IO ()
deploymentGcAssembly conf = do
  putStrLn "Initiating garbage collection..."
  allDeps <- Dep.getDeploymentsInt (Config.haldPath conf) (Config.bootPath conf)
  Lock.umountDirForcibly Lock.Simple $ Config.haldPath conf
  Space.gcBroken allDeps (Config.rootDir conf) (Config.haldPath conf) (Config.bootPath conf)
  newAllDeps <- Dep.getDeploymentsInt (Config.haldPath conf) (Config.bootPath conf)
  Space.rmDeps
    (Config.keepDeps conf)
    newAllDeps
    (Config.rootDir conf)
    (Config.haldPath conf)
    (Config.bootPath conf)
  Lock.roBindMountDirToSelf Lock.Ro $ Config.haldPath conf
