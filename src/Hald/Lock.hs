{-# LANGUAGE CApiFFI #-}

module Hald.Lock where

import Control.Exception (IOException, bracket, catch)
import Control.Monad (unless, void, when)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt (..), CLong (..), CULong (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (poke)
import Hald.Util (TreeAction (..), WalkStrategy (..))
import Hald.Util qualified as Util
import System.Posix.Files (accessTimeHiRes, fileGroup, fileMode, fileOwner, getFileStatus, modificationTimeHiRes, setFileMode, setFileTimesHiRes, setOwnerAndGroup)
import System.Process (readProcess)

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

setImmutable :: FilePath -> IO ()
setImmutable fp = Util.ioOrPass $ setFileFlag fp fsImmutableFl

setMutable :: FilePath -> IO ()
setMutable fp = Util.ioOrPass $ setFileFlag fp 0

clearRecursiveImmutable :: FilePath -> IO ()
clearRecursiveImmutable fp = Util.ioOrPass $ Util.walk (ParallelN 4) action fp
  where
    action =
      TreeAction
        { dirAction = \p _ -> setFileFlag p 0,
          symAction = \_ _ -> return (),
          fileAction = \p _ -> setFileFlag p 0
        }

roBindMountDirToSelf :: ReadMode -> FilePath -> IO ()
roBindMountDirToSelf readMode dirPath =
  catch
    ( do
        mounted <- Util.isMountpoint dirPath
        unless mounted $
          void $
            readProcess
              "mount"
              ["-o", "bind," <> show readMode, "--make-private", dirPath, dirPath]
              ""
    )
    ( \e ->
        let err = show (e :: IOException)
         in putStrLn $ "Failed to bind mount " <> dirPath <> "; " <> err
    )

roRemountDir :: ReadMode -> FilePath -> IO ()
roRemountDir readMode dirPath =
  catch
    ( do
        mounted <- Util.isMountpoint dirPath
        when mounted $
          void $
            readProcess "mount" ["-o", "remount," <> show readMode, dirPath] ""
    )
    ( \e ->
        let err = show (e :: IOException)
         in putStrLn $ "Failed to remount mount " <> dirPath <> "; " <> err
    )

umountDirForcibly :: RecursiveUmount -> FilePath -> IO ()
umountDirForcibly opts dirPath = do
  mounted <- Util.isMountpoint dirPath
  when mounted $
    Util.ioOrPass $
      void $
        readProcess "umount" ["-" <> show opts, dirPath] ""

foreign import capi "linux/fs.h value FS_IOC_SETFLAGS"
  fsIocSetflags :: CULong

foreign import capi "linux/fs.h value FS_IMMUTABLE_FL"
  fsImmutableFl :: CInt

foreign import capi "fcntl.h open"
  c_open :: CString -> CInt -> IO CInt

foreign import capi "unistd.h close"
  c_close :: CInt -> IO CInt

foreign import capi "sys/ioctl.h ioctl"
  c_ioctl :: CInt -> CULong -> Ptr CLong -> IO CInt

foreign import capi "sys/ioctl.h ioctl"
  c_ioctl_int :: CInt -> CULong -> CInt -> IO CInt

setFileFlag :: FilePath -> CInt -> IO ()
setFileFlag path flag =
  withCString path $ \cpath -> do
    fd <- c_open cpath 0
    when (fd >= 0) $ do
      alloca $ \p -> do
        poke p (fromIntegral flag :: CLong)
        _ <- c_ioctl fd fsIocSetflags p
        return ()
      _ <- c_close fd
      return ()

foreign import capi "linux/fs.h value FICLONE"
  ficloneRequest :: CULong

ficlone :: FilePath -> FilePath -> IO Bool
ficlone src dst = clone `catch` \(_ :: IOException) -> return False
  where
    clone = bracket (openRead src) cClose $ \s ->
      if s < 0
        then return False
        else bracket (openWrite dst) cClose $ \d ->
          if d < 0
            then return False
            else do
              r <- c_ioctl_int d ficloneRequest (fromIntegral s)
              return (r == 0)
    openRead p = withCString p $ \c -> c_open c 0
    openWrite p = withCString p $ \c -> c_open c 1
    cClose fd = when (fd >= 0) $ void $ c_close fd

copyMetadata :: FilePath -> FilePath -> IO ()
copyMetadata src dst = do
  st <- getFileStatus src
  Util.ioOrPass $ setOwnerAndGroup dst (fileOwner st) (fileGroup st)
  setFileMode dst (fileMode st)
  Util.ioOrPass $ setFileTimesHiRes dst (accessTimeHiRes st) (modificationTimeHiRes st)
