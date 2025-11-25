module Main.Fail where

import Control.Concurrent qualified as Conc
import Control.Exception (catch)
import Control.Monad (unless)
import Data.Maybe qualified
import GHC.IO.Exception (IOException)
import Main.Config qualified as Config
import Main.Container qualified as Container
import Main.Deployment qualified as Dep
import Main.Lock qualified as Lock
import Main.Space qualified as Space
import System.Posix.Signals (Handler (..), Signal, SignalInfo (siginfoSignal), installHandler, signalProcess)
import System.Process (Pid, callCommand, getCurrentPid)

failAndCleanup :: Dep.Deployment -> Config.Config -> IO ()
failAndCleanup dep conf = do
  unless (Dep.identifier dep == -1) $ Space.rmDep dep conf
  Container.umountContainer "ald-root"
  Container.rmContainer "ald-root"
  Lock.roBindMountDirToSelf Lock.Ro $ Config.haldPath conf

genericTerminationHandler :: Config.Config -> Pid -> Maybe Dep.Deployment -> Handler
genericTerminationHandler conf pid dep = CatchInfoOnce $ \signalInfo -> do
  putStrLn $
    "Caught signal "
      <> show (siginfoSignal signalInfo)
      <> "\nExecution interrupted!"
      <> "\nAttempting cleanup of broken deployments..."
  catch
    (callCommand ("umount -Rfl " <> Config.haldPath conf <> "/" <> show (Dep.identifier pending) <> Config.bootPath conf <> " 2>/dev/null"))
    ( \e ->
        let _ = show (e :: IOException)
         in putStr ""
    )
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
