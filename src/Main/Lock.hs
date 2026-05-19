module Main.Lock where

import Control.Exception (IOException, catch)
import Control.Monad (forM_, unless, when)
import Data.Bits (shiftL, (.|.))
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt (..), CLong (..), CULong (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (poke, sizeOf)
import System.Directory (doesDirectoryExist, listDirectory, pathIsSymbolicLink)
import System.FilePath ((</>))
import System.Process (callCommand)

fsIocSetflags :: CULong
fsIocSetflags =
  (1 `shiftL` 30)
    .|. (fromIntegral (fromEnum 'f') `shiftL` 8)
    .|. (fromIntegral (2 :: CInt) `shiftL` 0)
    .|. (fromIntegral (sizeOf (undefined :: CLong)) `shiftL` 16)

fsImmutableFl :: CInt
fsImmutableFl = 0x00000010

fsAppendFl :: CInt
fsAppendFl = 0x00000020

foreign import ccall unsafe "fcntl.h open"
  c_open :: CString -> CInt -> IO CInt

foreign import ccall unsafe "unistd.h close"
  c_close :: CInt -> IO CInt

foreign import ccall unsafe "sys/ioctl.h ioctl"
  c_ioctl :: CInt -> CULong -> Ptr CLong -> IO CInt

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
setImmutable fp =
  catch
    (setFileFlag fp fsImmutableFl)
    (\e -> let _ = show (e :: IOException) in return ())

setMutable :: FilePath -> IO ()
setMutable fp =
  catch
    (setFileFlag fp 0)
    (\e -> let _ = show (e :: IOException) in return ())

setAppendOnly :: FilePath -> IO ()
setAppendOnly fp =
  catch
    (setFileFlag fp fsAppendFl)
    (\e -> let _ = show (e :: IOException) in return ())

removeAppendOnly :: FilePath -> IO ()
removeAppendOnly fp =
  catch
    (setFileFlag fp 0)
    (\e -> let _ = show (e :: IOException) in return ())

clearRecursiveImmutable :: FilePath -> IO ()
clearRecursiveImmutable fp =
  catch
    ( do
        isLink <- pathIsSymbolicLink fp
        unless isLink $ do
          isDir <- doesDirectoryExist fp
          if isDir
            then do
              setFileFlag fp 0
              contents <- listDirectory fp
              forM_ contents $ \name ->
                clearRecursiveImmutable (fp </> name)
            else
              setFileFlag fp 0
    )
    (\e -> let _ = show (e :: IOException) in return ())

roBindMountDirToSelf :: ReadMode -> FilePath -> IO ()
roBindMountDirToSelf readMode dirPath =
  catch
    ( callCommand
        ( "mountpoint "
            <> dirPath
            <> " >/dev/null 2>&1 || mount -o bind,"
            <> show readMode
            <> " --make-private "
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
            <> " >/dev/null 2>&1 && mount -o remount,"
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
            <> " >/dev/null 2>&1 && umount -"
            <> show opts
            <> " "
            <> dirPath
        )
    )
    ( \e ->
        let _ = show (e :: IOException)
         in return ()
    )
