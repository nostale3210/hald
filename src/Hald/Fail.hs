module Hald.Fail
  ( installAsyncHandler,
    cleanupOnError,
    failAndCleanup,
  )
where

import Control.Concurrent (myThreadId, throwTo)
import Control.Exception (AsyncException (UserInterrupt))
import Control.Monad (unless)
import Data.Maybe qualified
import Hald.Cas.Gc qualified as CasGc
import Hald.Config qualified as Config
import Hald.Container qualified as Container
import Hald.Deployment qualified as Dep
import Hald.Lock qualified as Lock
import Hald.Space qualified as Space
import Hald.Util qualified as Util
import System.FilePath ((</>))
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
  Lock.umountDirForcibly Lock.Rfl $ Config.haldPath conf </> show (Dep.identifier pending)
  deployments <- Dep.getDeploymentsInt conf
  Space.gcBroken deployments conf
  failAndCleanup pending conf

failAndCleanup :: Dep.Deployment -> Config.Config -> IO ()
failAndCleanup dep conf = do
  unless (Dep.identifier dep == -1) $ Space.rmDep dep conf
  CasGc.restoreStoreFlags conf
  Container.umountContainer "ald-root"
  Container.rmContainer "ald-root"
  Lock.roBindMountDirToSelf Lock.Ro $ Config.haldPath conf
