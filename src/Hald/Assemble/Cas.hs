module Hald.Assemble.Cas (gcAssembly, fsverityAssembly, verifyAssembly) where

import Hald.Assemble.Common qualified as Asm
import Hald.Cas.Gc qualified as CasGc
import Hald.Cas.Verify qualified as CasVer
import Hald.Config qualified as Config
import Hald.Deployment qualified as Dep
import Hald.Lock qualified as Lock
import Hald.Util qualified as Util
import System.Exit (exitFailure)

gcAssembly :: Config.Config -> Util.MessageContainer -> IO ()
gcAssembly conf msgCont = do
  Util.printProgress msgCont "Performing CAS garbage collection..."
  allDeps <- Dep.getDeploymentsInt conf
  Asm.withHaldStoreUnmounted conf Lock.Simple $ do
    CasGc.collectGarbage conf allDeps
    CasGc.restoreStoreFlags conf

fsverityAssembly :: Config.Config -> Util.MessageContainer -> IO ()
fsverityAssembly conf msgCont = do
  Util.printProgress msgCont "Enabling fs-verity on all CAS objects..."
  Asm.withHaldStoreUnmounted conf Lock.Simple $
    CasGc.enableFsVerityOnCas conf

verifyAssembly :: Config.Config -> Util.MessageContainer -> Dep.Deployment -> IO ()
verifyAssembly conf msgCont dep = do
  Util.printProgress msgCont $
    "Verifying integrity of deployment "
      <> show (Dep.identifier dep)
      <> "..."
  digest <- CasVer.getDeploymentDigest conf dep
  case digest of
    Nothing ->
      Util.printInfo
        ("Couldn't verify integrity of deployment " <> show (Dep.identifier dep) <> "!")
        (Config.interactive conf)
        >> exitFailure
    Just hash -> Util.printInfo ("Digest: " <> hash) $ Config.interactive conf
