module Hald.Activate where

import Control.Exception (IOException, bracketOnError, try)
import Control.Monad (unless, void, when)
import Hald.Deployment qualified as Dep
import Hald.Lock qualified as Lock
import Hald.Util qualified as Util
import System.Directory (removePathForcibly)
import System.FilePath ((</>))
import System.Posix.Signals (addSignal, blockSignals, emptySignalSet, sigINT, sigTERM, unblockSignals)
import System.Process (readProcess)
import UnliftIO.Async (concurrently)

getNewRoot :: Dep.Deployment -> IO FilePath
getNewRoot nextDep =
  case Dep.rootDir nextDep of
    Nothing -> Util.fatalWith "nextDep doesn't supply rootDir" ""
    Just x -> return x

moveMountBeneath :: FilePath -> FilePath -> IO ()
moveMountBeneath fromPath toPath =
  void $ Util.quietReadProcess "mount" ["--move", "--beneath", fromPath, toPath] ""

legacyMoveMountBeneath :: FilePath -> FilePath -> IO ()
legacyMoveMountBeneath fromPath toPath =
  void $ Util.quietReadProcess "move-mount" ["-mb", fromPath, toPath] ""

ensureOverlayEmptyDir :: FilePath -> IO ()
ensureOverlayEmptyDir hp = do
  let emptyDir = hp <> "/empty"
  Util.ensureDirExists emptyDir
  dirContents <- Util.listDirSafe emptyDir
  unless (null dirContents) $ do
    Lock.setMutable emptyDir
    mapM_ (removePathForcibly . (emptyDir </>)) dirContents
  Lock.setImmutable emptyDir

usrOverlayMount :: FilePath -> FilePath -> FilePath -> IO ()
usrOverlayMount hp fromPath toPath = do
  ensureOverlayEmptyDir hp
  let emptyDir = hp <> "/empty"
  Util.ioOrDie "Mounting overlay usr" $
    void $
      readProcess
        "mount"
        [ "-t",
          "overlay",
          "usr-root",
          "--make-private",
          "-o",
          "lowerdir=" <> fromPath <> ":" <> emptyDir,
          toPath
        ]
        ""

privateMount :: FilePath -> IO ()
privateMount path =
  Util.ioOrDie "Making mount private" $
    void $
      readProcess "mount" ["--make-private", path] ""

bindMount :: FilePath -> FilePath -> IO ()
bindMount fromPath toPath =
  Util.ioOrDie "Binding mount" $
    void $
      readProcess "mount" ["-o", "bind", "--make-private", fromPath, toPath] ""

prepareMounts :: FilePath -> FilePath -> FilePath -> Bool -> Bool -> IO ()
prepareMounts root hp newRoot usrMounted etcMounted = do
  when usrMounted $ do
    privateMount (root <> "/usr")
    usrOverlayMount hp (newRoot <> "/usr") (newRoot <> "/usr")
  when etcMounted $ do
    privateMount (root <> "/etc")
    bindMount (newRoot <> "/etc") (newRoot <> "/etc")

releasePrepMounts :: FilePath -> Bool -> Bool -> IO ()
releasePrepMounts newRoot usrMounted etcMounted = do
  when usrMounted $ Lock.umountDirForcibly Lock.Simple (newRoot <> "/usr")
  when etcMounted $ Lock.umountDirForcibly Lock.Simple (newRoot <> "/etc")

releaseActiveMounts :: FilePath -> Bool -> Bool -> IO ()
releaseActiveMounts root usrMounted etcMounted = do
  when usrMounted $ Lock.umountDirForcibly Lock.Fl (root <> "/usr")
  when etcMounted $ Lock.umountDirForcibly Lock.Fl (root <> "/etc")

swapMountsBeneath :: FilePath -> FilePath -> FilePath -> Bool -> Bool -> Bool -> IO ()
swapMountsBeneath root hp newRoot usrMounted etcMounted useBeneath =
  let move
        | useBeneath = moveMountBeneath
        | otherwise = legacyMoveMountBeneath
   in bracketOnError
        (prepareMounts root hp newRoot usrMounted etcMounted)
        (\_ -> releasePrepMounts newRoot usrMounted etcMounted)
        ( \_ -> do
            void $
              concurrently
                (when usrMounted $ move (newRoot <> "/usr") (root <> "/usr"))
                (when etcMounted $ move (newRoot <> "/etc") (root <> "/etc"))
            releaseActiveMounts root usrMounted etcMounted
        )

activateNewRoot :: FilePath -> FilePath -> Dep.Deployment -> IO ()
activateNewRoot root hp newDep = do
  oldId <- Dep.getCurrentDeploymentId root
  let newId = Dep.identifier newDep
      signals = addSignal sigTERM . addSignal sigINT $ emptySignalSet
  when (oldId /= newId) $ do
    newRoot <- getNewRoot newDep
    usrMounted <- Util.isMountpoint $ root <> "/usr"
    etcMounted <- Util.isMountpoint $ root <> "/etc"
    useBeneath <- Util.hasMountBeneath

    privateMount hp
    blockSignals signals

    result <- try @IOException $ do
      unless usrMounted $ usrOverlayMount hp (newRoot <> "/usr") (root <> "/usr")
      unless etcMounted $ bindMount (newRoot <> "/etc") (root <> "/etc")
      when (usrMounted || etcMounted) $
        swapMountsBeneath root hp newRoot usrMounted etcMounted useBeneath

    unblockSignals signals

    case result of
      Right _ -> return ()
      Left e -> do
        releasePrepMounts newRoot usrMounted etcMounted
        Util.fatal $ "Activating deployment failed: " <> show e
