module Main.Util where

import Control.Concurrent (forkIO)
import Control.Concurrent qualified as Conc
import Control.Concurrent.STM qualified as Stm
import Control.Exception.Base (IOException, catch)
import Control.Monad (forM, unless, when)
import Data.ByteString.Char8 qualified as C
import GHC.IO.Exception (ExitCode (ExitSuccess))
import System.Directory
import System.Environment (getArgs, getExecutablePath)
import System.FileLock (SharedExclusive (Exclusive), lockFile, tryLockFile)
import System.FilePath (takeDirectory)
import System.Posix (exitImmediately, getRealUserID, raiseSignal, sigINT)
import System.Process (callCommand)

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
ensureDirExists dir = do
  doesDirectoryExist dir >>= \dirExistence ->
    unless dirExistence $
      catch
        (createDirectoryIfMissing True dir)
        ( \e ->
            let err = show (e :: IOException)
             in printInfo ("Couldn't create missing directory " <> dir <> "; " <> err) False
        )

recursiveFileSearch :: FilePath -> FilePath -> IO [FilePath]
recursiveFileSearch rootDir fileName = do
  dirContents <- listDirectory rootDir
  matches <- forM dirContents recSearch
  return (concat matches)
  where
    recSearch fp = do
      let path = rootDir <> "/" <> fp
      pathIsDir <- doesDirectoryExist path
      if not pathIsDir
        && fileName == fp
        then return [path]
        else
          if pathIsDir
            then recursiveFileSearch path fileName
            else return []

relabelSeLinuxPath :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
relabelSeLinuxPath rootPath contexts relabelDir bp = do
  catch
    ( callCommand
        ( "restorecon -RF "
            <> bp
        )
    )
    ( \e ->
        let err = show (e :: IOException)
         in printInfo "Relabeling boot directory failed." False
              >> error err
    )
  catch
    ( callCommand
        ( "setfiles -F -T \"$((\"$(nproc --all)\"/2))\" -r "
            <> rootPath
            <> " "
            <> contexts
            <> " "
            <> relabelDir
            <> " 2>/dev/null"
        )
    )
    ( \e ->
        let err = show (e :: IOException)
         in printInfo "Relabeling root directory failed." False
              >> error err
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

signKernel :: FilePath -> Int -> IO Bool
signKernel bp dep =
  let kernelPath = bp <> "/" <> show dep <> "/vmlinuz"
   in pathExists kernelPath >>= \kernelExists ->
        ( if kernelExists
            then
              catch
                ( callCommand
                    ("sbctl sign " <> kernelPath <> " &>/dev/null")
                    >> return True
                )
                ( \e ->
                    let _ = show (e :: IOException)
                     in return False
                )
            else error $ "Kernel for deployment " <> show dep <> " doesn't seem to exist."
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
checkInteractive =
  catch
    ( callCommand
        "{ infocmp 2>/dev/null | grep -q smcup ; } && { infocmp 2>/dev/null | grep -q rmcup ; }"
        >> return True
    )
    ( \e ->
        let _ = show (e :: IOException)
         in return False
    )

checkSystemdInhibit :: IO Bool
checkSystemdInhibit =
  catch
    ( callCommand "systemd-inhibit --who=\"hald\" --what=\"idle\" sleep 0.01 &>/dev/null"
        >> return True
    )
    ( \e ->
        let _ = show (e :: IOException)
         in return False
    )

execWithSystemdInhibit :: IO ()
execWithSystemdInhibit =
  getArgs >>= \cmdArgs ->
    getExecutablePath >>= \execPath ->
      catch
        ( callCommand
            ( "systemd-inhibit --what=\"idle:sleep:shutdown\" --who=\"ald-rootful-ops\" "
                <> "--why=\"Managing deployments\" -- "
                <> execPath
                <> " --skip-systemd-inhibit "
                <> unwords cmdArgs
            )
            >> exitImmediately ExitSuccess
        )
        ( \e ->
            let _ = show (e :: IOException)
             in raiseSignal sigINT
        )

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
