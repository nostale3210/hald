module Main.Container where

import Control.Exception (IOException, catch)
import Control.Monad (void)
import Main.Config qualified as Config
import Main.Util qualified as Util
import System.Process (callProcess, readProcess)

mountContainer :: String -> String -> IO FilePath
mountContainer podName podUri =
  catch
    (void $ Util.quietReadProcess "podman" ["create", "--replace", "--name", podName, podUri] "")
    (\e -> Util.fatal $ "Creating container failed: " <> show (e :: IOException))
    >> catch
      (readProcess "podman" ["mount", "ald-root"] [])
      (\e -> Util.fatalWith ("Mounting container failed: " <> show (e :: IOException)) "failed")
    >>= \podMount -> return $ Util.removeString "\n" podMount

umountContainer :: String -> IO ()
umountContainer podName =
  catch
    (void $ Util.quietReadProcess "podman" ["unmount", podName] "")
    ( \e ->
        let err = show (e :: IOException)
         in Util.printInfo ("Unmounting container " <> podName <> " unsuccessful\n" <> err) False
    )

rmContainer :: String -> IO ()
rmContainer podName =
  catch
    (void $ Util.quietReadProcess "podman" ["rm", "-f", podName] "")
    ( \e ->
        let err = show (e :: IOException)
         in Util.printInfo ("Removing container " <> podName <> " unsuccessful\n" <> err) False
    )

buildImage :: Config.Config -> IO ()
buildImage conf =
  catch
    ( do
        Util.printInfo "Building custom container image..." (Config.interactive conf)
        callProcess
          "podman"
          [ "build",
            "--isolation=chroot",
            "--build-arg=SOURCE_IMAGE=" <> Config.containerUri conf,
            "-t",
            Config.localTag conf,
            Config.configPath conf
          ]
        Util.printInfo "Container build completed." (Config.interactive conf)
    )
    (\e -> Util.fatal $ "Building image failed: " <> show (e :: IOException))

pullImage :: Config.Config -> IO Bool
pullImage conf = do
  let uri = Config.containerUri conf
  curDigest <-
    catch
      (readProcess "podman" ["inspect", "--format", "'{{.Digest}}'", uri] [])
      (\e -> let _ = show (e :: IOException) in return "")
  upstrDigest <-
    catch
      (readProcess "skopeo" ["inspect", "--format", "'{{.Digest}}'", "docker://" <> uri] [])
      (\e -> let _ = show (e :: IOException) in return "")
  if curDigest == upstrDigest
    then Util.printInfo "Latest image already pulled" (Config.interactive conf) >> return False
    else do
      Util.printInfo ("Pulling image " <> uri <> "...") (Config.interactive conf)
      catch
        (callProcess "podman" ["pull", uri])
        (\e -> Util.fatal $ "Pulling image failed: " <> show (e :: IOException))
      Util.printInfo "Image pull completed." (Config.interactive conf)
      return True
