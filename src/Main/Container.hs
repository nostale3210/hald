module Main.Container where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, catch)
import Main.Util qualified as Util
import System.IO (hPutStrLn, stderr)
import System.Posix.Signals (raiseSignal, sigTERM)
import System.Process (callCommand, readProcess)

mountContainer :: String -> String -> IO FilePath
mountContainer podName podUri =
  catch
    (callCommand ("podman create --replace --name " <> podName <> " " <> podUri <> " >/dev/null 2>&1"))
    ( \e ->
        hPutStrLn stderr ("Creating container failed: " <> show (e :: IOException))
          >> raiseSignal sigTERM
          >> threadDelay maxBound
    )
    >> catch
      (readProcess "podman" ["mount", "ald-root"] [])
      ( \e ->
          hPutStrLn stderr ("Mounting container failed: " <> show (e :: IOException))
            >> raiseSignal sigTERM
            >> threadDelay maxBound
            >> return "failed"
      )
    >>= \podMount -> return $ Util.removeString "\n" podMount

umountContainer :: String -> IO ()
umountContainer podName =
  catch
    ( callCommand
        ("podman unmount " <> podName <> " >/dev/null 2>&1")
    )
    ( \e ->
        let err = show (e :: IOException)
         in Util.printInfo ("Unmounting container " <> podName <> " unsuccessful\n" <> err) False
    )

rmContainer :: String -> IO ()
rmContainer podName =
  catch
    ( callCommand
        ("podman rm -f " <> podName <> " >/dev/null 2>&1")
    )
    ( \e ->
        let err = show (e :: IOException)
         in Util.printInfo ("Removing container " <> podName <> " unsuccessful\n" <> err) False
    )

buildImage :: String -> String -> FilePath -> IO ()
buildImage podUri locTag confPath =
  catch
    ( callCommand
        ( "podman build --isolation=chroot --build-arg=SOURCE_IMAGE="
            <> podUri
            <> " -t "
            <> locTag
            <> " "
            <> confPath
        )
    )
    ( \e ->
        hPutStrLn stderr ("Building image failed: " <> show (e :: IOException))
          >> raiseSignal sigTERM
          >> threadDelay maxBound
    )

pullImage :: String -> IO Bool
pullImage podUri = do
  curDigest <-
    catch
      (readProcess "podman" ["inspect", "--format", "'{{.Digest}}'", podUri] [])
      (\e -> let _ = show (e :: IOException) in return "")
  upstrDigest <-
    catch
      (readProcess "skopeo" ["inspect", "--format", "'{{.Digest}}'", "docker://" <> podUri] [])
      (\e -> let _ = show (e :: IOException) in return "")
  if curDigest == upstrDigest
    then Util.printInfo "Latest image already pulled" False >> return False
    else do
      catch
        (callCommand ("podman pull " <> podUri))
        ( \e ->
            hPutStrLn stderr ("Pulling image failed: " <> show (e :: IOException))
              >> raiseSignal sigTERM
              >> threadDelay maxBound
        )
      return True
