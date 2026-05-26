module Main.Util where

import Control.Concurrent (forkIO, getNumCapabilities, threadDelay)
import Control.Concurrent qualified as Conc
import Control.Concurrent.STM qualified as Stm
import Control.Exception (IOException, catch)
import Control.Monad (forM, unless, void, when)
import Data.ByteString.Char8 qualified as C
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesPathExist, findExecutable, listDirectory)
import System.Environment (getArgs, getExecutablePath)
import System.Exit (ExitCode (..))
import System.FileLock (SharedExclusive (Exclusive), lockFile, tryLockFile)
import System.FilePath (takeDirectory, (</>))
import System.IO (hIsTerminalDevice, hPutStrLn, stderr, stdout)
import System.IO.Error (isAlreadyExistsError)
import System.Posix (executeFile, getRealUserID, raiseSignal, sigINT, sigTERM)
import System.Posix.Files (FileStatus, createSymbolicLink, getSymbolicLinkStatus)
import System.Process (CreateProcess (..), StdStream (NoStream), proc, readCreateProcessWithExitCode, readProcess)

data MessageContainer
  = MessageContainer {interactive :: Bool, channel :: Stm.TChan String}

removeString :: String -> String -> String
removeString mtch = replaceString mtch ""

replaceString :: String -> String -> String -> String
replaceString mtch rplc str =
  case str of
    [] -> []
    [x] ->
      if [x] == mtch
        then rplc
        else [x]
    x : xs ->
      if take ln str == mtch
        then replaceString mtch rplc (rplc <> drop ln str)
        else x : replaceString mtch rplc xs
  where
    ln = length mtch

takeUntil :: String -> Char -> String
takeUntil input stopChar =
  case input of
    [x] -> if x == stopChar then "" else [x]
    x : xs ->
      if x == stopChar
        then ""
        else [x] <> takeUntil xs stopChar
    _ -> input

newIdentifier :: [Int] -> Int
newIdentifier idents =
  case idents of
    [] -> 0
    _ : _ -> succ $ maximum idents

pathExists :: FilePath -> IO Bool
pathExists path =
  catch
    (doesPathExist path)
    (\e -> let _ = show (e :: IOException) in return False)

ensureDirExists :: FilePath -> IO ()
ensureDirExists dir =
  doesDirectoryExist dir >>= \dirExistence ->
    unless dirExistence $
      catch
        (createDirectoryIfMissing True dir)
        ( \e ->
            let err = show (e :: IOException)
             in printInfo ("Couldn't create missing directory " <> dir <> "; " <> err) False
        )

createSymlink :: FilePath -> FilePath -> IO ()
createSymlink target link =
  catch
    (createSymbolicLink target link)
    ( \e ->
        if isAlreadyExistsError e
          then return ()
          else
            let err = show (e :: IOException)
             in printInfo
                  ( "Couldn't create symlink from " <> target <> " to " <> link <> "; " <> err
                  )
                  False
    )

isMountpoint :: FilePath -> IO Bool
isMountpoint path =
  catch
    (quietReadProcess "mountpoint" ["-q", path] "" >> return True)
    ( \e ->
        let _ = show (e :: IOException)
         in return False
    )

quietReadProcess :: FilePath -> [String] -> String -> IO String
quietReadProcess cmd args input = do
  (ec, out, _) <-
    readCreateProcessWithExitCode
      (proc cmd args) {std_err = NoStream}
      input
  case ec of
    ExitSuccess -> return out
    ExitFailure n -> ioError (userError (cmd <> " failed with exit code " <> show n))

recursiveFileSearch :: FilePath -> FilePath -> IO [FilePath]
recursiveFileSearch rootDir fileName = do
  dirContents <- listDirectory rootDir
  matches <- forM dirContents recSearch
  return (concat matches)
  where
    recSearch fp = do
      let path = rootDir </> fp
      pathIsDir <- doesDirectoryExist path
      if not pathIsDir
        && fileName == fp
        then return [path]
        else
          if pathIsDir
            then recursiveFileSearch path fileName
            else return []

relabelSeLinuxPath :: FilePath -> FilePath -> FilePath -> IO ()
relabelSeLinuxPath rootPath contexts bp = do
  catch
    (void $ quietReadProcess "restorecon" ["-RF", bp] "")
    ( \e ->
        let _ = show (e :: IOException)
         in hPutStrLn stderr "Relabeling boot directory failed."
              >> raiseSignal sigTERM
              >> threadDelay maxBound
    )
  catch
    ( do
        createSymlink "usr/lib" (rootPath <> "/lib")
        createSymlink "usr/lib64" (rootPath <> "/lib64")
        threads <- getNumCapabilities
        catch
          (void $ quietReadProcess "chroot" [rootPath, "/usr/bin/setfiles", "-F", "-T", show threads, contexts, "/"] "")
          (\e -> let _ = show (e :: IOException) in return ())
    )
    ( \e ->
        let _ = show (e :: IOException)
         in hPutStrLn stderr "Relabeling root directory failed."
              >> raiseSignal sigTERM
              >> threadDelay maxBound
    )

getUserId :: IO Int
getUserId =
  getRealUserID >>= \uid ->
    return (read (show uid) :: Int)

rootCheck :: IO Bool
rootCheck =
  getUserId >>= \uid ->
    return $ uid == 0

acquireLock :: FilePath -> IO Bool
acquireLock fp = do
  ensureDirExists $ takeDirectory fp
  lockRes <- tryLockFile fp Exclusive
  case lockRes of
    Just _ -> return True
    Nothing -> do
      C.putStrLn $ C.pack "Waiting to acquire lock..."
      _ <- lockFile fp Exclusive
      return True

signKernel :: FilePath -> Int -> FilePath -> IO Bool
signKernel bp dep target =
  let kernelPath = bp </> show dep <> target
   in pathExists kernelPath >>= \kernelExists ->
        ( if kernelExists
            then
              catch
                (quietReadProcess "sbctl" ["sign", kernelPath] "" >> return True)
                ( \e ->
                    let _ = show (e :: IOException)
                     in return False
                )
            else
              hPutStrLn stderr ("Kernel for deployment " <> show dep <> " doesn't seem to exist.")
                >> raiseSignal sigTERM
                >> threadDelay maxBound
                >> return False
        )

genericRootfulPreproc :: FilePath -> Bool -> Bool -> IO MessageContainer
genericRootfulPreproc lockPath interactive inhibit = do
  isRoot <- rootCheck
  unless isRoot $ error "This action needs elevated privileges!"

  systemdAvailable <- checkSystemdInhibit
  when (systemdAvailable && inhibit) execWithSystemdInhibit

  isLocked <- acquireLock lockPath
  unless isLocked $ error "Couldn't acquire lock!"

  msgChannel <- Stm.atomically Stm.newTChan
  let msgCont = MessageContainer {interactive = interactive, channel = msgChannel}
  _ <- forkIO (printChannelMsg (channel msgCont) $ C.pack "|/-\\")
  return msgCont

checkInteractive :: IO Bool
checkInteractive = hIsTerminalDevice stdout

checkSystemdInhibit :: IO Bool
checkSystemdInhibit = do
  r <- findExecutable "systemd-inhibit"
  return $ case r of
    Just _ -> True
    Nothing -> False

execWithSystemdInhibit :: IO ()
execWithSystemdInhibit =
  getArgs >>= \cmdArgs ->
    getExecutablePath >>= \execPath ->
      catch
        ( executeFile
            "systemd-inhibit"
            True
            ( "--what=idle:sleep:shutdown"
                : "--who=ald-rootful-ops"
                : "--why=Managing deployments"
                : "--"
                : execPath
                : "--skip-systemd-inhibit"
                : cmdArgs
            )
            Nothing
        )
        ( \e ->
            let _ = show (e :: IOException)
             in raiseSignal sigINT
        )

setSystemThreads :: Int -> IO ()
setSystemThreads maxThreads = do
  nproc <-
    catch
      (readProcess "nproc" [] "")
      (\e -> let _ = (e :: IOException) in return "1")
  let threads = read nproc :: Int
  Conc.setNumCapabilities $ min threads maxThreads

printProgress :: MessageContainer -> String -> IO ()
printProgress msgCont status =
  if interactive msgCont
    then Stm.atomically $ Stm.writeTChan (channel msgCont) status
    else C.putStrLn . C.pack $ "[Progress] " <> status

printInfo :: String -> Bool -> IO ()
printInfo status interactive =
  if interactive
    then C.putStrLn . C.pack $ "\r\ESC[K[Info] " <> status
    else C.putStrLn . C.pack $ "[Info] " <> status

printChannelMsg :: Stm.TChan String -> C.ByteString -> IO ()
printChannelMsg channel bar = do
  status <- Stm.atomically $ Stm.readTChan channel
  C.putStr $! C.pack "\r\ESC[K[" <> C.pack [C.head bar] <> C.pack "] " <> C.pack status
  Conc.threadDelay 100000
  channelEmpty <- Stm.atomically $ Stm.isEmptyTChan channel
  when channelEmpty (Stm.atomically $ Stm.unGetTChan channel status)
  printChannelMsg channel (C.append (C.tail bar) (C.pack [C.head bar]))

listDirSafe :: FilePath -> IO [FilePath]
listDirSafe dir = catch (listDirectory dir) (\(_ :: IOException) -> return [])

tryStat :: FilePath -> IO (Maybe FileStatus)
tryStat path =
  catch
    (Just <$> getSymbolicLinkStatus path)
    (\e -> let _ = show (e :: IOException) in return Nothing)
