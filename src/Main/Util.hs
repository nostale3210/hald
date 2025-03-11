module Main.Util where

import Control.Concurrent (forkIO)
import Control.Concurrent qualified as Conc
import Control.Concurrent.STM qualified as Stm
import Control.Exception.Base (IOException, catch)
import Control.Monad (forM, unless, when)
import Data.ByteString.Char8 qualified as C
import System.Directory
import System.FileLock (SharedExclusive (Exclusive), lockFile, tryLockFile)
import System.FilePath (takeDirectory)
import System.Posix (getRealUserID)
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
    ( \e -> do
        let _ = show (e :: IOException)
        return False
    )

ensureDirExists :: FilePath -> IO ()
ensureDirExists dir = do
  dirExistence <- doesDirectoryExist dir
  unless dirExistence $
    catch
      (createDirectoryIfMissing True dir)
      ( \e -> do
          let err = show (e :: IOException)
          printInfo ("Couldn't create missing directory " <> dir <> "; " <> err) False
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
    ( \e -> do
        let err = show (e :: IOException)
        printInfo "Relabeling boot directory failed." False
        error err
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
    ( \e -> do
        let err = show (e :: IOException)
        printInfo "Relabeling root directory failed." False
        error err
    )

getUserId :: IO Int
getUserId = do
  uid <- getRealUserID
  return (read (show uid) :: Int)

rootCheck :: IO Bool
rootCheck = do
  uid <- getUserId
  return $ uid == 0

acquireLock :: FilePath -> IO Bool
acquireLock fp = do
  ensureDirExists $ takeDirectory fp
  lockRes <- tryLockFile fp Exclusive
  case lockRes of
    Just _ -> return True
    Nothing -> do
      putStrLn "Waiting to acquire lock..."
      _ <- lockFile fp Exclusive
      return True

genericRootfulPreproc :: FilePath -> Bool -> IO MessageContainer
genericRootfulPreproc lockPath interactive = do
  isRoot <- rootCheck
  unless isRoot $ error "This action needs elevated privileges!"

  isLocked <- acquireLock lockPath
  unless isLocked $ error "Couldn't acquire lock!"

  msgChannel <- Stm.atomically Stm.newTChan
  let msgCont = MessageContainer {interactive = interactive, channel = msgChannel}
  _ <- forkIO (printChannelMsg (channel msgCont) $ C.pack "|/-\\")
  return msgCont

checkInteractive :: IO Bool
checkInteractive =
  catch
    ( do
        callCommand
          "{ infocmp 2>/dev/null | grep -q smcup ; } && { infocmp 2>/dev/null | grep -q rmcup ; }"
        return True
    )
    ( \e -> do
        let _ = show (e :: IOException)
        return False
    )

printProgress :: MessageContainer -> String -> IO ()
printProgress msgCont status = do
  if interactive msgCont
    then Stm.atomically $ Stm.writeTChan (channel msgCont) status
    else putStrLn $ "[Progress] " <> status

printInfo :: String -> Bool -> IO ()
printInfo status interactive =
  if interactive
    then putStrLn $ "\r\ESC[K[Info] " <> status
    else putStrLn $ "[Info] " <> status

printChannelMsg :: Stm.TChan String -> C.ByteString -> IO ()
printChannelMsg channel bar = do
  status <- Stm.atomically $ Stm.readTChan channel
  C.putStr $! C.pack "\r\ESC[K[" <> C.pack [C.head bar] <> C.pack "] " <> C.pack status
  Conc.threadDelay 100000
  channelEmpty <- Stm.atomically $ Stm.isEmptyTChan channel
  when channelEmpty (Stm.atomically $ Stm.unGetTChan channel status)
  printChannelMsg channel (C.append (C.tail bar) (C.pack [C.head bar]))
