module Run where
import Guard  ( Guard(..), File(..), Dir , runGuard)
import qualified Filesystem.Path as FP
import Data.Text                 (unpack,pack)
import Filesystem.Path.CurrentOS (toText,fromText)
import System.FSNotify           (withManager,watchTree,Event(Modified))
import System.FilePath.Glob      (matchWith,matchPosix,compile)

--FSNotify uses Filesystem.Path so this normalizes the filepaths
mkFP :: FP.FilePath -> Prelude.FilePath
mkFP file = either unpack unpack $ toText file

--find the Guard that watches a file event only suppots modified for now
modEvent :: [Guard] -> Event -> IO ()
modEvent guards (Modified file _) = mapM_ find guards  where
    f = mkFP file
    find g | checkFile g f = runGuard g f
           | otherwise     = return ()
modEvent _      _                 = return ()

watch1 :: Guard -> IO ()
watch1 g = watch "." [g]

watch :: Dir -> [Guard] -> IO ()
watch d g = withManager $ \man -> do
    putStrLn "Ready"
    _ <- watchTree man (fromText $ pack d) (const True) $ modEvent g
    _ <- getLine
    return ()

--checks if a filen is guarded or if it's part of a pattern that is
checkFile :: Guard -> FilePath -> Bool
checkFile (Guard fs _) file = any check fs where
    check (File    f) = f == file
    check (Pattern p) = p ~= file

(~=) :: String -> FilePath -> Bool
p ~= file = matchWith matchPosix (compile p) file
