module Main.Assemble.Activate (deploymentActivationAssembly) where

import Main.Activate qualified as Activate
import Main.Config qualified as Config
import Main.Deployment qualified as Dep
import Main.Lock qualified as Lock

deploymentActivationAssembly :: Int -> Config.Config -> IO ()
deploymentActivationAssembly newDepId conf = do
  putStrLn $ "Activating deployment " <> show newDepId <> "..."
  newDep <-
    Dep.getDeployment
      newDepId
      (Config.rootDir conf)
      (Config.haldPath conf)
      (Config.bootPath conf)
  let root = Config.rootDir conf <> "/"
  Lock.unlockRoot root
  Lock.umountDirForcibly Lock.Simple $ Config.haldPath conf
  Lock.umountDirForcibly Lock.Rfl $ root <> "usr"
  Activate.activateNewRoot
    (Config.rootDir conf)
    newDep
    (Config.haldPath conf)
  Lock.roBindMountDirToSelf Lock.Ro $ root <> "usr"
  Lock.roBindMountDirToSelf Lock.Rw $ root <> "usr/local"
  Lock.roBindMountDirToSelf Lock.Ro $ Config.haldPath conf
  Lock.lockRoot root
