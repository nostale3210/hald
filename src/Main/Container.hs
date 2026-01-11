module Main.Container where

import Control.Exception (IOException, catch)
import Main.Util qualified as Util
import System.Posix.Signals (raiseSignal, sigTERM)
import System.Process

mountContainer :: String -> String -> IO FilePath
mountContainer podName podUri =
  catch
    (callCommand ("podman create --replace --name " <> podName <> " " <> podUri <> " >/dev/null 2>&1"))
    (\e -> let err = show (e :: IOException) in error err)
    >> catch
      (readProcess "podman" ["mount", "ald-root"] [])
      (\e -> let err = show (e :: IOException) in error err)
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
    >> setImageEtcTime hp

syncImageStructure :: FilePath -> FilePath -> IO ()
syncImageStructure fp hp =
  catch
    ( callCommand
        ( "rsync -ac -f\"+ */\" -f\"- *\" --delete "
            <> fp
            <> "/usr "
            <> fp
            <> "/etc "
            <> hp
            <> "/image/ >/dev/null 2>&1"
        )
    )
    (\e -> let _ = show (e :: IOException) in raiseSignal sigTERM)

syncImageBatched :: FilePath -> FilePath -> IO ()
syncImageBatched fp hp =
  catch
    ( callCommand
        ( "find "
            <> fp
            <> "/usr "
            <> fp
            <> "/etc "
            <> "! -type d -printf \"%s\\t%p\\n\" | sort -nr | cut -f2- | "
            <> "sed \"s|^\\("
            <> fp
            <> "\\)|\\1/.|g\" | xargs -n5000 -P\"$((\"$(nproc --all)\"/2))\" "
            <> "bash -c 'rsync -aHlcx --delete --relative --ignore-missing-args \"$@\" "
            <> hp
            <> "/image/ >/dev/null 2>&1' _ >/dev/null 2>&1"
        )
    )
    (\e -> let _ = show (e :: IOException) in raiseSignal sigTERM)

trimImageLeftovers :: FilePath -> FilePath -> IO ()
trimImageLeftovers fp hp =
  catch
    ( callCommand
        ( "find "
            <> hp
            <> "/image/usr "
            <> hp
            <> "/image/etc "
            <> "| sed \"s|^"
            <> hp
            <> "/image||g\" | sort > /tmp/hp && "
            <> "find "
            <> fp
            <> "/usr "
            <> fp
            <> "/etc "
            <> "| sed \"s|^"
            <> fp
            <> "||g\" | sort > /tmp/fp && "
            <> "comm -23 /tmp/hp /tmp/fp | "
            <> "sed \"s|^|"
            <> hp
            <> "/image|g\" | xargs rm -rf >/dev/null 2>&1"
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
            <> "-execdir sh -c \"touch -d 1970-01-01T01:00:00 '{}' >/dev/null 2>&1 || :\" \\;"
        )
    )
    (\e -> let _ = show (e :: IOException) in putStrLn "ETCTime" >> raiseSignal sigTERM)
