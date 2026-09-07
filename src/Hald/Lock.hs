{-# LANGUAGE CApiFFI #-}

module Hald.Lock where

import Control.Exception (IOException, bracket, catch)
import Control.Monad (unless, void, when)
import Data.ByteString qualified as BS
import Data.Word (Word16, Word64)
import Foreign.C.Error (Errno (..), eEXIST, eNOSYS, eNOTTY, eOPNOTSUPP, getErrno)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt (..), CLong (..), CUInt (..), CULong (..))
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (poke, pokeByteOff)
import Hald.Mount qualified as Mount
import Hald.Util (TreeAction (..), WalkStrategy (..))
import Hald.Util qualified as Util
import System.Posix.Files (accessTimeHiRes, fileGroup, fileMode, fileOwner, fileSize, getFileStatus, modificationTimeHiRes, setFileMode, setFileTimesHiRes, setOwnerAndGroup)

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

umountDirForcibly :: RecursiveUmount -> FilePath -> IO ()
umountDirForcibly opts dirPath = do
  mounted <- Mount.isMountpoint dirPath
  when mounted
    $ Util.ioOrPass
    $ Util.runProcess_ "umount" ["-" <> show opts, dirPath]

foreign import capi "linux/fs.h value FS_IOC_SETFLAGS"
  fsIocSetflags :: CULong

foreign import capi "linux/fs.h value FS_IMMUTABLE_FL"
  fsImmutableFl :: CInt

foreign import capi "fcntl.h open"
  c_open :: CString -> CInt -> IO CInt

foreign import capi "unistd.h sysconf"
  c_sysconf :: CInt -> IO CLong

foreign import capi "unistd.h value _SC_PAGESIZE"
  c_SC_PAGESIZE :: CInt

foreign import capi "unistd.h close"
  c_close :: CInt -> IO CInt

foreign import capi "sys/ioctl.h ioctl"
  c_ioctl :: CInt -> CULong -> Ptr CLong -> IO CInt

foreign import capi "sys/ioctl.h ioctl"
  c_ioctl_int :: CInt -> CULong -> CInt -> IO CInt

foreign import capi "linux/fsverity.h value FS_IOC_ENABLE_VERITY"
  fsIocEnableVerity :: CULong

foreign import capi "linux/fsverity.h value FS_IOC_MEASURE_VERITY"
  fsIocMeasureVerity :: CULong

foreign import capi "sys/ioctl.h ioctl"
  c_ioctl_ptr :: CInt -> CULong -> Ptr () -> IO CInt

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

enableFsVerity :: FilePath -> IO ()
enableFsVerity path =
  Util.ioOrPass $
    withCString path $ \cpath -> do
      sz <- fileSize <$> getFileStatus path
      when (sz > 0) $
        bracket (c_open cpath 0) closeWhenOpen $ \fd ->
          when (fd >= 0) $
            allocaBytes fsverityArgSizeInt $ \arg -> do
              fillBytes arg 0 fsverityArgSizeInt
              pokeByteOff arg 0 verityVersion
              pokeByteOff arg 4 verityHashAlgSha256
              blockSize <- fromIntegral <$> c_sysconf c_SC_PAGESIZE
              pokeByteOff arg 8 (blockSize :: CUInt)
              pokeByteOff arg 16 (0 :: Word64)
              pokeByteOff arg 32 (0 :: Word64)
              r <- c_ioctl_ptr fd fsIocEnableVerity arg
              when (r < 0) $ do
                e <- getErrno
                unless (e `elem` silentErrnos) $
                  Util.printInfo ("fsverity enable failed on " <> path <> "; errno " <> show (errnoCode e)) False
  where
    fsverityArgSizeInt = 128 :: Int
    verityVersion = 1 :: CUInt
    verityHashAlgSha256 = 1 :: CUInt
    silentErrnos = [eEXIST, eNOSYS, eNOTTY, eOPNOTSUPP]
    errnoCode (Errno n) = fromIntegral n
    closeWhenOpen fd
      | fd >= 0 = void $ c_close fd
      | otherwise = return ()

measureFsVerity :: FilePath -> IO (Maybe BS.ByteString)
measureFsVerity path =
  bracket (openRead path) c_close $ \fd ->
    if fd < 0
      then return Nothing
      else allocaBytes 36 $ \arg -> do
        fillBytes arg 0 36
        pokeByteOff arg 0 verityVersion
        pokeByteOff arg 2 digestSize
        r <- c_ioctl_ptr fd fsIocMeasureVerity arg
        if r == 0
          then Just <$> BS.packCStringLen (castPtr (arg `plusPtr` 4), 32)
          else return Nothing
  where
    verityVersion = 1 :: Word16
    digestSize = 32 :: Word16
    openRead p = withCString p $ \c -> c_open c 0
