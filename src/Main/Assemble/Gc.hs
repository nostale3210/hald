module Main.Assemble.Gc where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM qualified as Stm
import Control.Monad (unless)
import Data.ByteString.Char8 qualified as C
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
  msgChannel <- Stm.atomically Stm.newTChan
  let msgCont = Util.MessageContainer {Util.interactive = Config.interactive conf, Util.channel = msgChannel}
  _ <- forkIO (Util.printChannelMsg (Util.channel msgCont) $ C.pack "|/-\\")
  deploymentGcAssembly conf msgCont

deploymentGcAssembly :: Config.Config -> Util.MessageContainer -> IO ()
deploymentGcAssembly conf msgCont = do
  Util.printProgress msgCont "Performing garbage collection..."

  allDeps <- Dep.getDeploymentsInt (Config.haldPath conf) (Config.bootPath conf)
  Lock.umountDirForcibly Lock.Simple $ Config.haldPath conf
  Space.gcBroken allDeps conf
  newAllDeps <- Dep.getDeploymentsInt (Config.haldPath conf) (Config.bootPath conf)
  Space.rmDeps (Config.keepDeps conf) newAllDeps conf
  Lock.roBindMountDirToSelf Lock.Ro $ Config.haldPath conf
