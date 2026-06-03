module Hald.Lock where

import Control.Exception (IOException, bracket, catch)
import Control.Monad (unless, void, when)
import Data.Bits (shiftL, (.|.))
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt (..), CLong (..), CULong (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (poke, sizeOf)
import Hald.Util (TreeAction (..), WalkStrategy (..))
import Hald.Util qualified as Util
import System.Process (readProcess)

fsIocSetflags :: CULong
fsIocSetflags =
  (1 `shiftL` 30)
    .|. (fromIntegral (fromEnum 'f') `shiftL` 8)
    .|. (fromIntegral (2 :: CInt) `shiftL` 0)
    .|. (fromIntegral (sizeOf (undefined :: CLong)) `shiftL` 16)

fsImmutableFl :: CInt
fsImmutableFl = 0x00000010

foreign import ccall unsafe "fcntl.h open"
  c_open :: CString -> CInt -> IO CInt

foreign import ccall unsafe "unistd.h close"
  c_close :: CInt -> IO CInt

foreign import ccall unsafe "sys/ioctl.h ioctl"
  c_ioctl :: CInt -> CULong -> Ptr CLong -> IO CInt

foreign import ccall unsafe "sys/ioctl.h ioctl"
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

ficloneRequest :: CULong
ficloneRequest =
  (1 `shiftL` 30) .|. (4 `shiftL` 16) .|. (0x94 `shiftL` 8) .|. 9

-- FICLONE = _IOW(0x94, 9, int) from <linux/fs.h>

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
