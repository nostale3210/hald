module Hald.Mount where

import Control.Monad (unless, when)
import Hald.Util qualified as Util

data ReadMode
  = Ro
  | Rw

instance Show ReadMode where
  show Ro = "ro"
  show Rw = "rw"

isMountpoint :: FilePath -> IO Bool
isMountpoint path =
  Util.ioOrDefault False $ Util.quietReadProcess "mountpoint" ["-q", path] "" >> return True

runMount :: [String] -> IO ()
runMount = Util.runProcess_ "mount"

roBindMountDirToSelf :: ReadMode -> FilePath -> IO ()
roBindMountDirToSelf readMode dirPath =
  Util.catchInfoPrint
    False
    ("Failed to bind mount " <> dirPath)
    $ do
      mounted <- isMountpoint dirPath
      unless mounted $
        runMount
          ["-o", "bind," <> show readMode, "--make-private", dirPath, dirPath]

roRemountDir :: ReadMode -> FilePath -> IO ()
roRemountDir readMode dirPath =
  Util.catchInfoPrint
    False
    ("Failed to remount mount " <> dirPath)
    $ do
      mounted <- isMountpoint dirPath
      when mounted $
        runMount
          ["-o", "remount," <> show readMode, dirPath]

privateMount :: FilePath -> IO ()
privateMount path =
  Util.ioOrDie "Making mount private" $
    runMount ["--make-private", path]

bindMount :: FilePath -> FilePath -> IO ()
bindMount fromPath toPath =
  Util.ioOrDie "Binding mount" $
    runMount ["-o", "bind", "--make-private", fromPath, toPath]

usrOverlayMount :: FilePath -> FilePath -> FilePath -> IO ()
usrOverlayMount hp fromPath toPath =
  Util.ioOrDie "Mounting overlay usr" $
    runMount
      [ "-t",
        "overlay",
        "usr-root",
        "--make-private",
        "-o",
        "lowerdir=" <> fromPath <> ":" <> hp <> "/empty",
        toPath
      ]

moveMountBeneath :: FilePath -> FilePath -> IO ()
moveMountBeneath fromPath toPath =
  runMount ["--move", "--beneath", fromPath, toPath]

legacyMoveMountBeneath :: FilePath -> FilePath -> IO ()
legacyMoveMountBeneath fromPath toPath =
  Util.runProcess_ "move-mount" ["-mb", fromPath, toPath]
