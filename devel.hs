{-# LANGUAGE PackageImports #-}
import "league-famous" Application (getApplicationDev)
import Network.Wai.Handler.Warp
    (runSettings, defaultSettings, settingsPort)
import System.Directory (doesFileExist, removeFile)
import System.Exit (exitSuccess)
import Control.Concurrent (threadDelay, forkIO)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    let confName = case args of
                     [arg] -> arg
                     _ -> "Development"

    putStrLn "Starting devel application"
    (port, app) <- getApplicationDev (read confName)
    forkIO $ runSettings defaultSettings
        { settingsPort = port
        } app
    loop

loop :: IO ()
loop = do
  threadDelay 100000
  e <- doesFileExist "dist/devel-terminate"
  if e then terminateDevel else loop

terminateDevel :: IO ()
terminateDevel = exitSuccess
