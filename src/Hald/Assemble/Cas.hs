module Hald.Assemble.Cas (gcAssembly, fsverityAssembly, verifyAssembly, verifyDigest) where

import Control.Monad (when)
import Data.List (isPrefixOf)
import Data.Maybe (listToMaybe)
import Hald.Assemble.Common qualified as Asm
import Hald.Cas.Gc qualified as CasGc
import Hald.Cas.Verify qualified as CasVer
import Hald.Config qualified as Config
import Hald.Deployment qualified as Dep
import Hald.Lock qualified as Lock
import Hald.Pe qualified as Pe
import Hald.Util qualified as Util

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
  verifyDigest conf dep Nothing

verifyDigest :: Config.Config -> Dep.Deployment -> Maybe String -> IO ()
verifyDigest conf newDep mdigest = do
  let interactive = Config.interactive conf
      bootContext = Config.rootDir conf /= ""
      safeDigest = if bootContext then mdigest else Nothing
  case safeDigest of
    Just d -> verify newDep d interactive
    Nothing -> case Dep.ukiPath (Dep.bootComponents newDep) of
      Nothing -> return ()
      Just ukiPath -> do
        extCmdline <- Pe.extractCmdline ukiPath
        case extCmdline of
          Nothing -> Util.printInfo "No .cmdline section found in UKI, skipping verification" interactive
          Just cmdline ->
            case extractDigestParam cmdline of
              Nothing ->
                Util.printInfo "No hald.digest= parameter in cmdline, skipping verification" interactive
              Just expected ->
                verify newDep expected interactive
  where
    extractDigestParam = listToMaybe . map (drop 12) . filter (isPrefixOf "hald.digest=") . words
    verify dep expected interactive = do
      Util.printInfo ("Expected deployment digest: " <> expected) interactive
      mCurrent <- CasVer.getDeploymentDigest conf dep
      case mCurrent of
        Nothing ->
          Util.fatal "Couldn't calculate deployment digest, but one is expected"
        Just actual -> do
          Util.printInfo ("Deployment digest: " <> actual) interactive
          when (expected /= actual) $
            Util.fatal "Deployment digest mismatch"
          Util.printInfo "Digest verification successful" interactive
