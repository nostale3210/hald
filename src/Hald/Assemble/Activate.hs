module Hald.Assemble.Activate where

import Control.Monad (unless, when)
import Data.List (isPrefixOf)
import Data.Maybe (listToMaybe)
import Hald.Activate qualified as Activate
import Hald.Cas.Verify qualified as CasVer
import Hald.Config qualified as Config
import Hald.Deployment qualified as Dep
import Hald.Mount qualified as Mount
import Hald.Pe qualified as Pe
import Hald.Util qualified as Util

deploymentActivationAssembly :: Config.Config -> Util.MessageContainer -> Dep.Deployment -> IO ()
deploymentActivationAssembly conf msgCont newDep = do
  Util.printInfo ("Activating deployment " <> show (Dep.identifier newDep) <> "...") (Util.interactive msgCont)
  Util.printProgress msgCont ("Activating deployment " <> show (Dep.identifier newDep) <> "...")
  verifyDigest (Util.interactive msgCont) conf newDep
  Activate.ensureOverlayEmptyDir (Config.haldPath conf)
  haldMounted <- Mount.isMountpoint (Config.haldPath conf)
  unless
    haldMounted
    (Mount.roBindMountDirToSelf Mount.Ro $ Config.haldPath conf)
  Activate.activateNewRoot
    (Config.rootDir conf)
    (Config.haldPath conf)
    newDep
  Mount.roRemountDir Mount.Rw $ root <> "etc"
  where
    root = Config.rootDir conf <> "/"

verifyDigest :: Bool -> Config.Config -> Dep.Deployment -> IO ()
verifyDigest interactive conf newDep =
  case Dep.ukiPath (Dep.bootComponents newDep) of
    Nothing -> return ()
    Just ukiPath -> do
      mCmdline <- Pe.extractCmdline ukiPath
      case mCmdline of
        Nothing ->
          Util.printInfo "No .cmdline section found in UKI, skipping verification" interactive
        Just cmdline ->
          case extractDigestParam cmdline of
            Nothing ->
              Util.printInfo "No hald.digest= parameter in cmdline, skipping verification" interactive
            Just expected -> do
              Util.printInfo ("Expected deployment digest: " <> expected) interactive
              mCurrent <- CasVer.getDeploymentDigest conf newDep
              case mCurrent of
                Nothing ->
                  Util.fatal "Couldn't calculate deployment digest, but one is expected"
                Just actual -> do
                  Util.printInfo ("Deployment digest: " <> actual) interactive
                  when (expected /= actual) $
                    Util.fatal "Deployment digest mismatch"
                  Util.printInfo "Digest verification successful" interactive
  where
    extractDigestParam = listToMaybe . map (drop 12) . filter (isPrefixOf "hald.digest=") . words
