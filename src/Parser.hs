module Parser where

import Cmd(Cmd,wrapCmd)
import Guard(Guard(..),File(..),Dir,canonicalGuard,fileOrPattern)
import Control.Applicative
import Control.Monad.Trans (liftIO)
import System.Directory    (canonicalizePath)
import System.FilePath     ((</>),takeDirectory)
import Text.Parsec         (many1,ParsecT,option)
import Text.Parsec.Prim    (runPT)
import Text.Parsec.Char    (noneOf)
import Text.Parsec.Error   (ParseError)
import qualified Text.Parsec.Token as TP

type Parser a = ParsecT String () IO a

lang :: TP.GenLanguageDef String () IO
lang = TP.LanguageDef { TP.commentStart    = ""
                      , TP.commentEnd      = ""
                      , TP.commentLine     = "#"
                      , TP.nestedComments  = False
                      , TP.identStart      = noneOf " \n" --file names
                      , TP.identLetter     = TP.identStart lang
                      , TP.opStart         = undefined
                      , TP.opLetter        = undefined
                      , TP.reservedOpNames = undefined
                      , TP.reservedNames   = [ "srcDir:", "rule:"
                                             , "cmd:"   , "watch:"
                                             ]
                      , TP.caseSensitive   = False
                      }

lexer :: TP.GenTokenParser String () IO
lexer = TP.makeTokenParser lang

fileP :: Parser FilePath
fileP = TP.identifier lexer

whiteSpace :: Parser ()
whiteSpace = TP.whiteSpace lexer

reserved :: String -> Parser ()
reserved = TP.reserved lexer

srcGuardP :: FilePath -> Parser (Dir,[Guard])
srcGuardP f = do
    whiteSpace
    src    <- optionalSrc f
    rules  <- many1 rule
    rules' <- liftIO $ mapM (canonicalGuard src) rules
    return (src,rules')

optionalSrc :: FilePath -> Parser Dir
optionalSrc f = option "." $ do
    src <- srcDir
    liftIO $ canonicalizePath $ src </> f

srcDir :: Parser FilePath
srcDir = reserved "srcDir:" >> fileP

rule :: Parser Guard
rule = do
    (reserved "rule:")
    Guard <$> many1 watchFile <*> command

watchFile :: Parser File
watchFile = reserved "watch:" *> (fileOrPattern <$> fileP)

command :: Parser Cmd
command = reserved "cmd:" *> (wrapCmd <$> (many1 $ noneOf "#\n") <* whiteSpace)

parseFromFile :: Parser a -> String -> IO (Either ParseError a)
parseFromFile p fname = readFile fname >>= runPT p () fname

parseFile :: FilePath -> IO (Dir,[Guard])
parseFile f = do
    res <- flip parseFromFile f $  srcGuardP $ takeDirectory f
    either (error . show) return res
