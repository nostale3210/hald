module Main.Container where

import Control.Exception (IOException, catch)
import Main.Util qualified as Util
import System.Posix.Signals (raiseSignal, sigTERM)
import System.Process

mountContainer :: String -> String -> IO FilePath
mountContainer podName podUri =
  catch
    (callCommand ("podman create --replace --name " <> podName <> " " <> podUri <> " &>/dev/null"))
    (\e -> let err = show (e :: IOException) in error err)
    >> catch
      (readProcess "podman" ["mount", "ald-root"] [])
      (\e -> let err = show (e :: IOException) in error err)
    >>= \podMount -> return $ Util.removeString "\n" podMount

umountContainer :: String -> IO ()
umountContainer podName =
  catch
    ( callCommand
        ("podman unmount " <> podName <> " &>/dev/null")
    )
    ( \e ->
        let err = show (e :: IOException)
         in Util.printInfo ("Unmounting container " <> podName <> " unsuccessful\n" <> err) False
    )

rmContainer :: String -> IO ()
rmContainer podName =
  catch
    ( callCommand
        ("podman rm -f " <> podName <> " &>/dev/null")
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
    (\e -> let _ = show (e :: IOException) in raiseSignal sigTERM)

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
        (\e -> let _ = show (e :: IOException) in raiseSignal sigTERM)
      return True

syncImage :: FilePath -> FilePath -> IO ()
syncImage fp hp =
  Util.ensureDirExists (hp <> "/image")
    >> Util.ensureDirExists (hp <> "/empty")
    >> syncImageStructure fp hp
    >> syncImageBatched fp hp
    >> trimImageLeftovers fp hp

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
    (\e -> let _ = show (e :: IOException) in raiseSignal sigTERM)

syncImageBatched :: FilePath -> FilePath -> IO ()
syncImageBatched fp hp =
  catch
    ( callCommand
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
    (\e -> let _ = show (e :: IOException) in raiseSignal sigTERM)

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
    (\e -> let _ = show (e :: IOException) in raiseSignal sigTERM)

setImageEtcTime :: FilePath -> IO ()
setImageEtcTime hp =
  catch
    ( callCommand
        ( "find "
            <> hp
            <> "/image/etc -mindepth 1 "
            <> "-execdir sh -c \"touch -d @0 '{}' &>/dev/null || :\" \\;"
        )
    )
    (\e -> let _ = show (e :: IOException) in raiseSignal sigTERM)
