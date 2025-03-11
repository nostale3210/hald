module Main.Fail where

import Control.Monad (unless)
import Data.Maybe qualified
import Main.Config qualified as Config
import Main.Container qualified as Container
import Main.Deployment qualified as Dep
import Main.Space qualified as Space
import System.Posix.Signals (Handler (..), Signal, SignalInfo (siginfoSignal), installHandler, signalProcess)
import System.Process (Pid, getCurrentPid)

failAndCleanup :: Dep.Deployment -> Config.Config -> IO ()
failAndCleanup dep conf = do
  unless (Dep.identifier dep == -1) $ Space.rmDep dep conf
  Container.umountContainer "ald-root"
  Container.rmContainer "ald-root"

genericTerminationHandler :: Config.Config -> Pid -> Maybe Dep.Deployment -> Handler
genericTerminationHandler conf pid dep = CatchInfoOnce $ \signalInfo -> do
  putStrLn $
    "Caught signal "
      <> show (siginfoSignal signalInfo)
      <> "\nExecution interrupted!"
      <> "\nAttempting cleanup of broken deployments..."
  deployments <-
    Dep.getDeploymentsInt
      (Config.haldPath conf)
      (Config.bootPath conf)
  Space.gcBroken deployments conf
  failAndCleanup pending conf
  signalProcess (siginfoSignal signalInfo) pid
  where
    pending = Data.Maybe.fromMaybe Dep.dummyDeployment dep

installGenericHandler :: [Signal] -> Config.Config -> Maybe Dep.Deployment -> IO ()
installGenericHandler signals conf pending =
  case signals of
    [] -> return ()
    signal : xs ->
      getCurrentPid >>= \pid ->
        installHandler
          signal
          (genericTerminationHandler conf pid pending)
          Nothing
          >> installGenericHandler xs conf pending
