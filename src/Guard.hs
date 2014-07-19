module Guard where

import Cmd (Cmd(..),runCmd,spliceFile)
import System.Directory (canonicalizePath)
import System.FilePath  ((</>))

--associate lists of files/patterns with a command
data Guard = Guard [File] Cmd deriving Show

type Dir = FilePath
data File = File FilePath
          | Pattern String -- glob pattern
          deriving Show

runGuard :: Guard -> FilePath -> IO ()
runGuard (Guard _ cmd) f = runCmd $ spliceFile f cmd

fileOrPattern :: String -> File
fileOrPattern f
  | any (`elem` "?*[") f = Pattern f --according to man 7 glob
  | otherwise       = File f

canonicalGuard :: Dir -> Guard -> IO Guard
canonicalGuard src (Guard fs c) = flip Guard c `fmap` mapM fixFile fs where
    fixFile (File f)    = File `fmap` canonicalizePath (src </> f)
    fixFile (Pattern p) = return $ Pattern $ src </> p
