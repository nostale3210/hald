module Hald.Assemble.Common (withRootfulAssembly, withHaldStoreUnmounted) where

import Control.Exception (bracket_, onException)
import Data.Maybe (fromMaybe)
import Hald.Config qualified as Config
import Hald.Deployment qualified as Dep
import Hald.Fail qualified as Fail
import Hald.Lock qualified as Lock
import Hald.Mount qualified as Mount
import Hald.Util qualified as Util
import System.Posix.Signals (sigINT, sigTERM)

withRootfulAssembly ::
  Config.Config ->
  Bool ->
  IO (Maybe Dep.Deployment) ->
  (Util.MessageContainer -> Dep.Deployment -> IO ()) ->
  IO ()
withRootfulAssembly conf inhibit depAction body = do
  msgCont <- Util.genericRootfulPreproc (Config.configPath conf <> "/.hald.lock") (Config.interactive conf) inhibit
  Fail.installAsyncHandler [sigINT, sigTERM]
  mDep <- depAction
  flip onException (Fail.cleanupOnError conf mDep (Just msgCont)) $
    body msgCont (fromMaybe Dep.dummyDeployment mDep)

withHaldStoreUnmounted :: Config.Config -> Lock.RecursiveUmount -> IO () -> IO ()
withHaldStoreUnmounted conf mode body =
  bracket_
    (Lock.umountDirForcibly mode $ Config.haldPath conf)
    (Mount.roBindMountDirToSelf Mount.Ro $ Config.haldPath conf)
    body
