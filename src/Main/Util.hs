module Main.Util where

import Control.Exception.Base (IOException, catch)
import Control.Monad (forM, unless)
import System.Directory
import System.FileLock (SharedExclusive (Exclusive), lockFile, tryLockFile)
import System.FilePath (takeDirectory, takeFileName)
import System.Posix (getRealUserID)
import System.Process (callCommand)

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
          putStrLn ("Couldn't create missing directory " <> dir <> "; " <> err)
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
  putStrLn $ "Relabeling deployment " <> takeFileName relabelDir <> "..."
  catch
    ( callCommand
        ( "restorecon -RF "
            <> bp
        )
    )
    ( \e -> do
        let err = show (e :: IOException)
        putStrLn "Relabeling boot directory failed."
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
        putStrLn "Relabeling root directory failed."
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
