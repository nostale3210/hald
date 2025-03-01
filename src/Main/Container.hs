module Main.Container where

import Control.Exception (IOException, catch)
import Main.Util qualified as Util
import System.Process

mountContainer :: String -> String -> IO FilePath
mountContainer podName podUri = do
  catch
    (callCommand ("podman create --replace --name " <> podName <> " " <> podUri <> " &>/dev/null"))
    ( \e -> do
        let err = show (e :: IOException)
        error err
    )
  podMount <-
    catch
      (readProcess "podman" ["mount", "ald-root"] [])
      ( \e -> do
          let err = show (e :: IOException)
          error err
      )
  return $ Util.removeString "\n" podMount

umountContainer :: String -> IO ()
umountContainer podName =
  catch
    ( callCommand
        ("podman unmount " <> podName <> " &>/dev/null")
    )
    ( \e -> do
        let err = show (e :: IOException)
        Util.printInfo ("Unmounting container " <> podName <> " unsuccessful; " <> err) False
    )

rmContainer :: String -> IO ()
rmContainer podName =
  catch
    ( callCommand
        ("podman rm -f " <> podName <> " &>/dev/null")
    )
    ( \e -> do
        let err = show (e :: IOException)
        Util.printInfo ("Removing container " <> podName <> " unsuccessful; " <> err) False
    )

buildImage :: String -> String -> FilePath -> IO ()
buildImage podUri locTag confPath =
  catch
    ( callCommand
        ("podman build --build-arg=SOURCE_IMAGE=" <> podUri <> " -t " <> locTag <> " " <> confPath)
    )
    ( \e -> do
        let err = show (e :: IOException)
        error err
    )

pullImage :: String -> IO Bool
pullImage podUri = do
  curDigest <-
    catch
      (readProcess "podman" ["inspect", "--format", "'{{.Digest}}'", podUri] [])
      ( \e -> do
          let err = show (e :: IOException)
          error err
      )
  upstrDigest <-
    catch
      (readProcess "skopeo" ["inspect", "--format", "'{{.Digest}}'", "docker://" <> podUri] [])
      ( \e -> do
          let _ = show (e :: IOException)
          return ""
      )
  if curDigest == upstrDigest
    then return False
    else do
      catch
        (callCommand ("podman pull " <> podUri))
        ( \e -> do
            let err = show (e :: IOException)
            error err
        )
      return True

syncImage :: FilePath -> FilePath -> IO ()
syncImage fp hp = do
  Util.ensureDirExists $ hp <> "/image"
  syncImageStructure fp hp
  syncImageBatched fp hp
  trimImageLeftovers fp hp

syncImageStructure :: FilePath -> FilePath -> IO ()
syncImageStructure fp hp =
  catch
    ( callCommand
        ( "rsync -ac -f\"+ */\" -f\"- *\" --delete "
            <> fp
            <> "/{usr,etc} "
            <> hp
            <> "/image/ &>/dev/null"
        )
    )
    ( \e -> do
        let err = show (e :: IOException)
        error err
    )

syncImageBatched :: FilePath -> FilePath -> IO ()
syncImageBatched fp hp = do
  pHandle <-
    catch
      ( spawnCommand
          ( "find "
              <> fp
              <> "/{usr,etc} ! -type d -printf \"%s\\t%p\\0\" | sort -znr | cut -z -f2- | "
              <> "sed -z \"s|^\\("
              <> fp
              <> "\\)|\\1/.|g\" | xargs -0 -n5000 -P\"$((\"$(nproc --all)\"/2))\" "
              <> "bash -c 'rsync -aHlcx --delete --relative \"$@\" "
              <> hp
              <> "/image/ &>/dev/null' _ &>/dev/null"
          )
      )
      ( \e -> do
          let err = show (e :: IOException)
          error err
      )
  _ <- waitForProcess pHandle
  return ()

trimImageLeftovers :: FilePath -> FilePath -> IO ()
trimImageLeftovers fp hp =
  catch
    ( callCommand
        ( "comm -z23 <(find "
            <> hp
            <> "/image/{usr,etc} -print0 | sed -z \"s|^"
            <> hp
            <> "/image||g\" | sort -z) "
            <> "<(find "
            <> fp
            <> "/{usr,etc} -print0 | sed -z \"s|^"
            <> fp
            <> "||g\" | sort -z) | "
            <> "sed -z \"s|^|"
            <> hp
            <> "/image|g\" | xargs -0 rm -rf &>/dev/null"
        )
    )
    ( \e -> do
        let err = show (e :: IOException)
        error err
    )
