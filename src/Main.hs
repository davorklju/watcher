import Prelude

import Guard(canonicalGuard,Guard(..),fileOrPattern,File(..))
import Cmd (Cmd,wrapCmd)
import Parser (parseFile)

import Run (watch,watch1)

import System.Console.GetOpt ( ArgDescr(..),OptDescr(..),getOpt
                      , usageInfo,ArgOrder(..))
import System.Environment    (getArgs)

data FileOrLocal = FP FilePath | Local | None

data Options = Options
             { optVersion :: Bool
             , optLocal   :: FileOrLocal
             , watchFiles :: [File]
             , optCmd     :: Maybe Cmd
             , optInit    :: Maybe FilePath
             }

defaultOpts :: Options
defaultOpts = Options
            { optVersion = False
            , watchFiles = []
            , optCmd     = Nothing
            , optLocal   = None
            , optInit    = Nothing
            }

push :: Options -> FilePath -> Options
push opt@Options{watchFiles = fs} f = opt{watchFiles = f':fs} where
    f' = fileOrPattern f

addCmd :: Options -> String -> Options
addCmd opt c = opt{optCmd = Just $ wrapCmd c}

setFile :: Options -> Maybe FilePath -> Options
setFile opt mf = opt{ optInit = Just $ maybe "." id $ mf }

options :: [OptDescr (Options -> Options)]
options = [ Option ['v'] ["version"]
                (NoArg $ \opt -> opt{optVersion = True})
                "display version number"
          , Option ['R'] ["rule"]
               (NoArg $ \opt -> opt{ optLocal = Local })
                "provide file to watch/command without loading a file"
          , Option ['w'] ["watch"]
                (flip ReqArg "file" $ \f opt -> push opt f)
                "provide file to watch only with --rule"
          , Option ['c'] ["cmd"]
                (flip ReqArg "command" $ \c opt -> addCmd opt c)
                "provide command to run only with --rule"
          , Option ['f'] ["file"]
                (flip ReqArg "file" $ \f opt -> opt{optLocal = FP f })
                "load source file"
          , Option ['r'] ["run"]
                (NoArg $ \opt -> opt{optLocal = FP ".local.guard"})
                "run optLocal guard file .optLocal.guard"
          , Option ['i'] ["init"]
                (flip OptArg "srcDir" $ \d opt -> setFile opt d)
                "create a .local.guard file with srcDir defaulted to '.'"
          ]

main :: IO ()
main = do
    args <- getArgs
    let (act,nonOpt,err) = getOpt RequireOrder options args
    case (nonOpt,err) of
        ([],[]) -> doArgs act
        ([],_)  -> putStrLn $ usageInfo "err" options
        (e:_,_) -> putStrLn $ flip usageInfo options $ "not an option " ++ e

doArgs :: [Options -> Options] -> IO ()
doArgs act = evalArgs $ foldr ($) defaultOpts act where
    evalArgs opt | optVersion opt         = putStrLn "0.1.0.0"
    evalArgs Options{optInit = Just file} = mkLocalFile file
    evalArgs opt =
        case optLocal opt of
            Local   -> runLocal opt
            FP file -> parseFile file >>= uncurry watch
            None    -> putStrLn $ usageInfo "" options

runLocal :: Options -> IO ()
runLocal opt | null $ watchFiles opt = putStrLn "expecting a file to watch"
runLocal opt = maybe err rg $ optCmd opt where
    err = putStrLn "expecting command"
    rg cmd = canonicalGuard "." (Guard fs cmd) >>= watch1
    fs     = watchFiles opt

mkLocalFile :: [Char] -> IO ()
mkLocalFile file = writeFile ".local.guard" $ concat
                 [ "srcDir: " , file
                 , " #start rules with rule:\n"
                 , "rule:\n"
                 , "\twatch: .local.guard"
                 , " # add files/glob to watch a watch:\n"
                 , "\tcmd:   echo @file"
                 , " # run this cammand with @file subing for the file name"
                 ]
