module Main.Assemble.Gc (deploymentGcAssembly) where

import Main.Config qualified as Config
import Main.Deployment qualified as Dep
import Main.Lock qualified as Lock
import Main.Space qualified as Space

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
