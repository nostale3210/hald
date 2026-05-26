module Main.Container where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, catch)
import Control.Monad (void)
import Main.Util qualified as Util
import System.IO (hPutStrLn, stderr)
import System.Posix.Signals (raiseSignal, sigTERM)
import System.Process (readProcess)

mountContainer :: String -> String -> IO FilePath
mountContainer podName podUri =
  catch
    (void $ readProcess "podman" ["create", "--replace", "--name", podName, podUri] "")
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
    (void $ readProcess "podman" ["unmount", podName] "")
    ( \e ->
        let err = show (e :: IOException)
         in Util.printInfo ("Unmounting container " <> podName <> " unsuccessful\n" <> err) False
    )

rmContainer :: String -> IO ()
rmContainer podName =
  catch
    (void $ readProcess "podman" ["rm", "-f", podName] "")
    ( \e ->
        let err = show (e :: IOException)
         in Util.printInfo ("Removing container " <> podName <> " unsuccessful\n" <> err) False
    )

buildImage :: String -> String -> FilePath -> IO ()
buildImage podUri locTag confPath =
  catch
    ( void $
        readProcess
          "podman"
          [ "build",
            "--isolation=chroot",
            "--build-arg=SOURCE_IMAGE=" <> podUri,
            "-t",
            locTag,
            confPath
          ]
          ""
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
        (void $ readProcess "podman" ["pull", podUri] "")
        ( \e ->
            hPutStrLn stderr ("Pulling image failed: " <> show (e :: IOException))
              >> raiseSignal sigTERM
              >> threadDelay maxBound
        )
      return True
