module Hald.Assemble.Common (withRootfulAssembly) where

import Control.Exception (onException)
import Data.Maybe (fromMaybe)
import Hald.Config qualified as Config
import Hald.Deployment qualified as Dep
import Hald.Fail qualified as Fail
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
  flip onException (Fail.cleanupOnError conf Nothing (Just msgCont)) $
    depAction >>= \mDep ->
      body msgCont (fromMaybe Dep.dummyDeployment mDep)
