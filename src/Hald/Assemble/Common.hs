module Hald.Assemble.Common (withRootful, withRootfulAssembly, withHaldStoreUnmounted) where

import Control.Exception (bracket_, onException)
import Data.Maybe (fromMaybe)
import Hald.Config qualified as Config
import Hald.Deployment qualified as Dep
import Hald.Fail qualified as Fail
import Hald.Lock qualified as Lock
import Hald.Mount qualified as Mount
import Hald.Util qualified as Util
import System.Posix.Signals (sigINT, sigTERM)

withRootful ::
  Config.Config ->
  Bool ->
  IO (Maybe Dep.Deployment) ->
  (Util.MessageContainer -> Dep.Deployment -> IO ()) ->
  IO ()
withRootful conf inhibit depAction body = do
  msgCont <- Util.genericRootfulPreproc (Config.configPath conf <> "/.hald.lock") (Config.interactive conf) inhibit
  mDep <- depAction
  body msgCont (fromMaybe Dep.dummyDeployment mDep)

withRootfulAssembly ::
  Config.Config ->
  Bool ->
  IO (Maybe Dep.Deployment) ->
  (Util.MessageContainer -> Dep.Deployment -> IO ()) ->
  IO ()
withRootfulAssembly conf inhibit depAction body = do
  Fail.installAsyncHandler [sigINT, sigTERM]
  withRootful conf inhibit depAction $ \msgCont dep ->
    flip onException (Fail.cleanupOnError conf (Just dep) (Just msgCont)) $
      body msgCont dep

withHaldStoreUnmounted :: Config.Config -> Lock.RecursiveUmount -> IO () -> IO ()
withHaldStoreUnmounted conf mode body =
  bracket_
    (Lock.umountDirForcibly mode $ Config.haldPath conf)
    (Mount.roBindMountDirToSelf Mount.Ro $ Config.haldPath conf)
    body
