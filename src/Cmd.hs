module Cmd where

import System.Process  (spawnCommand,waitForProcess)
import System.Exit     (ExitCode(ExitFailure))

newtype Cmd = Cmd [String]

wrapCmd :: String -> Cmd
wrapCmd = Cmd . words

instance Show Cmd where
    show (Cmd xs) = unwords xs

showCmd :: FilePath -> Cmd -> String
showCmd fp = show . spliceFile fp

spliceFile :: FilePath -> Cmd -> Cmd
spliceFile file (Cmd xs) = Cmd $ map idOrFile xs where
    idOrFile "@file" = file
    idOrFile x       = x

runCmd :: Cmd -> IO ()
runCmd cmd = do
    proc <- spawnCommand $ show cmd
    exit <- waitForProcess proc
    case exit of
        ExitFailure e -> print $ "failed with exit code " ++ show e
        _             -> return ()
