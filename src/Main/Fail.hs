module Main.Fail where

import Control.Exception (SomeException, catch)
import Control.Monad (unless)
import Data.Maybe qualified
import GHC.IO.Exception (IOException)
import Main.CAS.GC qualified as CasGc
import Main.Config qualified as Config
import Main.Container qualified as Container
import Main.Deployment qualified as Dep
import Main.Lock qualified as Lock
import Main.Space qualified as Space
import System.FilePath ((</>))
import System.Posix.Signals (Handler (..), Signal, SignalInfo (siginfoSignal), installHandler, signalProcess)
import System.Process (Pid, callCommand, getCurrentPid)

failAndCleanup :: Dep.Deployment -> Config.Config -> IO ()
failAndCleanup dep conf = do
  unless (Dep.identifier dep == -1) $ Space.rmDep dep conf
  CasGc.restoreStoreFlags conf
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
  _ <- catch cleanup (\(_ :: SomeException) -> return ())
  signalProcess (siginfoSignal signalInfo) pid
  where
    pending = Data.Maybe.fromMaybe Dep.dummyDeployment dep
    cleanup = do
      catch
        (callCommand ("umount -Rfl " <> Config.haldPath conf </> show (Dep.identifier pending) <> " 2>/dev/null"))
        (\(_ :: IOException) -> return ())
      deployments <- Dep.getDeploymentsInt conf
      Space.gcBroken deployments conf
      failAndCleanup pending conf

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
