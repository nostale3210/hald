module Hald.Fail
  ( installAsyncHandler,
    cleanupOnError,
    failAndCleanup,
  )
where

import Control.Concurrent (myThreadId, throwTo)
import Control.Exception (AsyncException (UserInterrupt))
import Control.Monad (unless, when)
import Data.Maybe qualified
import Hald.Cas.Gc qualified as CasGc
import Hald.Config qualified as Config
import Hald.Container qualified as Container
import Hald.Deployment qualified as Dep
import Hald.Lock qualified as Lock
import Hald.Mount qualified as Mount
import Hald.Space qualified as Space
import Hald.Util qualified as Util
import System.Posix.Signals (Handler (CatchInfoOnce), Signal, installHandler)

installAsyncHandler :: [Signal] -> IO ()
installAsyncHandler signals = do
  tid <- myThreadId
  mapM_ (\sig -> installHandler sig (CatchInfoOnce $ \_ -> throwTo tid UserInterrupt) Nothing) signals

cleanupOnError :: Config.Config -> Maybe Dep.Deployment -> Maybe Util.MessageContainer -> IO ()
cleanupOnError conf dep mMsgCont = do
  case mMsgCont of
    Just mc -> Util.printProgress mc "Fatal error. Cleaning up..."
    Nothing -> return ()
  let pending = Data.Maybe.fromMaybe Dep.dummyDeployment dep
      depRoot = Dep.rootDirFor conf (Dep.identifier pending)
  depRootExists <- Util.pathExists depRoot
  when depRootExists $ Lock.umountDirForcibly Lock.Rfl depRoot
  deployments <- Dep.getDeploymentsInt conf
  Space.gcBroken deployments conf
  failAndCleanup pending conf

failAndCleanup :: Dep.Deployment -> Config.Config -> IO ()
failAndCleanup dep conf = do
  unless (Dep.identifier dep == -1) $ Space.rmDep dep conf
  CasGc.restoreStoreFlags conf
  Container.umountContainer "hald-root"
  Container.rmContainer "hald-root"
  Mount.roBindMountDirToSelf Mount.Ro $ Config.haldPath conf
