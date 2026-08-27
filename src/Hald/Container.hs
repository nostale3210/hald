module Hald.Container where

import Control.Monad (void)
import Hald.Config qualified as Config
import Hald.Util qualified as Util
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.Process (callProcess, readProcess)

mountContainer :: String -> String -> IO FilePath
mountContainer podName podUri =
  Util.ioOrDie
    "Creating container"
    ( void $
        Util.quietReadProcess
          "podman"
          ["create", "--replace", "--name", podName, podUri]
          ""
    )
    >> Util.ioOrDie
      "Mounting container"
      (Util.quietReadProcess "podman" ["mount", "hald-root"] "")

umountContainer :: String -> IO ()
umountContainer podName =
  Util.catchInfoPrint
    False
    ("Unmounting container " <> podName <> " unsuccessful")
    (void $ Util.quietReadProcess "podman" ["unmount", podName] "")

rmContainer :: String -> IO ()
rmContainer podName =
  Util.catchInfoPrint
    False
    ("Removing container " <> podName <> " unsuccessful")
    (void $ Util.quietReadProcess "podman" ["rm", "-f", podName] "")

buildImage :: Config.Config -> IO ()
buildImage conf =
  Util.ioOrDie "Building container image" $ do
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

pullImage :: Config.Config -> IO Bool
pullImage conf = do
  let uri = Config.containerUri conf
  curDigest <-
    Util.ioOrDefault "" $
      readProcess "podman" ["inspect", "--format", "'{{.Digest}}'", uri] []
  upstrDigest <-
    Util.ioOrDefault "" $
      readProcess "skopeo" ["inspect", "--format", "'{{.Digest}}'", "docker://" <> uri] []
  if curDigest == upstrDigest
    then Util.printInfo "Latest image already pulled" (Config.interactive conf) >> return False
    else do
      Util.printInfo ("Pulling image " <> uri <> "...") (Config.interactive conf)
      Util.ioOrDie "Pulling image" $ callProcess "podman" ["pull", uri]
      Util.printInfo "Image pull completed." (Config.interactive conf)
      return True

getLayerInfo :: String -> IO [FilePath]
getLayerInfo name = do
  lower <-
    Util.ioOrDie
      "Getting container layer directories"
      ( Util.quietReadProcess
          "podman"
          ["inspect", "--format", "{{index .GraphDriver.Data \"LowerDir\"}}", name]
          ""
      )
  return $ filter (not . null) $ Util.splitOnChar ':' lower

findInLayers :: [FilePath] -> FilePath -> IO (Maybe FilePath)
findInLayers [] _ = return Nothing
findInLayers (d : ds) relPath = do
  let candidate = d </> relPath
  e <- doesFileExist candidate
  if e then return (Just candidate) else findInLayers ds relPath
