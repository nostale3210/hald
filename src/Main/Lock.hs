module Main.Lock where

import Control.Exception (IOException, catch)
import System.Process (callCommand)

data Immutable
  = P
  | M

instance Show Immutable where
  show P = "+"
  show M = "-"

data ReadMode
  = Ro
  | Rw

instance Show ReadMode where
  show Ro = "ro"
  show Rw = "rw"

data RecursiveUmount
  = Rfl
  | Simple
  | Fl

instance Show RecursiveUmount where
  show Rfl = "Rfl"
  show Simple = "f"
  show Fl = "fl"

setIPath :: Immutable -> FilePath -> IO ()
setIPath mode fp =
  catch
    ( callCommand
        ( "chattr "
            <> show mode
            <> "i "
            <> fp
        )
    )
    ( \e ->
        let err = show (e :: IOException)
         in putStrLn $ "Couldn't unlock /; " <> err
    )

roBindMountDirToSelf :: ReadMode -> FilePath -> IO ()
roBindMountDirToSelf readMode dirPath =
  catch
    ( callCommand
        ( "mountpoint "
            <> dirPath
            <> " &>/dev/null || mount -o bind,"
            <> show readMode
            <> " "
            <> dirPath
            <> " "
            <> dirPath
        )
    )
    ( \e ->
        let err = show (e :: IOException)
         in putStrLn $ "Failed to bind mount " <> dirPath <> "; " <> err
    )

roRemountDir :: ReadMode -> FilePath -> IO ()
roRemountDir readMode dirPath =
  catch
    ( callCommand
        ( "mountpoint "
            <> dirPath
            <> " &>/dev/null && mount -o remount,"
            <> show readMode
            <> " "
            <> dirPath
        )
    )
    ( \e ->
        let err = show (e :: IOException)
         in putStrLn $ "Failed to remount mount " <> dirPath <> "; " <> err
    )

umountDirForcibly :: RecursiveUmount -> FilePath -> IO ()
umountDirForcibly opts dirPath =
  catch
    ( callCommand
        ( "mountpoint "
            <> dirPath
            <> " &>/dev/null && umount -"
            <> show opts
            <> " "
            <> dirPath
        )
    )
    ( \e ->
        let _ = show (e :: IOException)
         in return ()
    )

unlockRoot :: FilePath -> IO ()
unlockRoot = setIPath M

lockRoot :: FilePath -> IO ()
lockRoot = setIPath P
