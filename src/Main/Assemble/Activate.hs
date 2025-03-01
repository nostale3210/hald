module Main.Assemble.Activate where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM qualified as Stm
import Control.Monad (unless)
import Data.ByteString.Char8 qualified as C
import Main.Activate qualified as Activate
import Main.Config qualified as Config
import Main.Deployment qualified as Dep
import Main.Lock qualified as Lock
import Main.Util qualified as Util

deploymentActivationAssemblyPre :: Int -> Config.Config -> IO ()
deploymentActivationAssemblyPre newDepId conf = do
  isRoot <- Util.rootCheck
  unless isRoot $ error "This action needs elevated privileges!"
  isLocked <- Util.acquireLock $ Config.configPath conf <> "/.hald.lock"
  unless isLocked $ error "Couldn't acquire lock!"
  msgChannel <- Stm.atomically Stm.newTChan
  let msgCont = Util.MessageContainer {Util.interactive = Config.interactive conf, Util.channel = msgChannel}
  _ <- forkIO (Util.printChannelMsg (Util.channel msgCont) $ C.pack "|/-\\")
  deploymentActivationAssembly newDepId conf msgCont

deploymentActivationAssembly :: Int -> Config.Config -> Util.MessageContainer -> IO ()
deploymentActivationAssembly newDepId conf msgCont = do
  Util.printInfo ("Activating deployment " <> show newDepId <> "...") (Util.interactive msgCont)
  Util.printProgress msgCont ("Activating deployment " <> show newDepId <> "...")

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
