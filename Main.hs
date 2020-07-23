{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import System.Environment
import Text.Printf
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Void
import Data.Word
import Data.List
import Control.Monad
import Data.Maybe
import Data.Char
import Data.Time.Clock.System
import Data.List.NonEmpty (NonEmpty (..))
import Text.ParserCombinators.ReadP (ReadP, readP_to_S)
import Text.Read.Lex (readHexP,readOctP)
import Data.Bifunctor

-- TODO/NOTES:
-- with the -v option, environment vars (might) get expanded
-- (but might need to increase string length to actually see)

main :: IO ()
main = do
    [straceFile] <- getArgs
    
    printf "Reading %s ..." straceFile    
    !raw <- Text.readFile straceFile
    printf " %d characters\n" (Text.length raw)

    let result1 = parse strace straceFile raw
    case result1 of
        Left bundle -> putStr (errorBundlePretty bundle)
        Right x -> print x

type Parser = Parsec Void Text

strace = manyTill traceLine eof

data TraceLine = TraceLine PID Timestamp Event
    deriving (Show)

data Event 
    = SystemCall SystemCallName [Text] Text
    | SystemCallUnfinished SystemCallName [Text]
    | SystemCallResumed SystemCallName [Text] Text
    | Signal SignalName Text
    | Killed SignalName
    | Exit Int
    deriving (Show)

newtype PID = PID Word32
    deriving (Show)

type Timestamp = SystemTime

newtype SystemCallName = SystemCallName Text
    deriving (Show)

newtype SignalName = SignalName Text
    deriving (Show)

traceLine :: Parser TraceLine
traceLine = do
    pid <- lexeme pid
    timestamp <- lexeme timestamp
    event <- choice [exit, killed, signal, systemCallResumed, systemCall]
    void eol <|> eof
    return $ TraceLine pid timestamp event

exit :: Parser Event
exit = Exit <$> (symbol "+++ exited with" *> lexeme L.decimal <* symbol "+++")

killed :: Parser Event
killed = Killed <$> (symbol "+++ killed by" *> lexeme signalName <* symbol "+++")

signal :: Parser Event
signal = do
    symbol "---"
    name <- lexeme signalName
    string <- Text.pack <$> manyTill anySingle " ---" <?> "signal string"
    return (Signal name string)

systemCallResumed :: Parser Event
systemCallResumed = do
    symbol "<..."
    name <- lexeme systemCallName
    symbol "resumed>"
    optional (symbol ",")
    args <- syscallArg `sepBy` (symbol ",")
    symbol ")"
    symbol "="
    retval <- takeWhileP (Just "return value") (/= '\n')    
    return $ SystemCallResumed name args retval

systemCall :: Parser Event
systemCall = do
    name <- lexeme systemCallName
    symbol "("
    args <- syscallArg `sepEndBy` (symbol ",")
    optional space1'  -- TODO: maybe simplify with just space
    unfinished <- optional "<unfinished ...>"
    case unfinished of
        Just _ -> return $ SystemCallUnfinished name args
        Nothing -> do
            symbol ")"
            symbol "="
            retval <- takeWhileP (Just "return value") (/= '\n')            
            return $ SystemCall name args retval

syscallArg :: Parser Text
syscallArg = choice
    [ stringLiteral
    , structLiteral
    , arrayLiteral
    , takeWhile1P (Just "argument") (\s -> s /= ',' && s /= ')' && s /= '<' && s /= '\n')
    ]

stringLiteral :: Parser Text
stringLiteral = label "string argument" $ do
    char '"'
    str <- manyTill cLiteral (char '"')
    trunc <- optional "..."
    case trunc of
        Nothing -> return $ Text.pack $ '"' : str ++ "\""
        Just _  -> return $ Text.pack $ '"' : str ++ "\"..."

cLiteral :: Parser Char
cLiteral = do
    s <- lookAhead (count' 1 10 anySingle)
    case readCChar s of
        Just (c, r) -> c <$ skipCount (length s - length r) anySingle
        Nothing -> unexpected (Tokens (head s:|[]))

readCChar :: String -> Maybe (Char, String)
readCChar s = case s of
    ('\\':c:r) -> case c of
        'a' -> Just ('\a',r)
        'b' -> Just ('\b',r)
        'e' -> Just ('\x1b',r)
        'f' -> Just ('\f',r)
        'n' -> Just ('\n',r)
        'r' -> Just ('\r',r)
        't' -> Just ('\t',r)
        'v' -> Just ('\v',r)
        '\\' -> Just ('\\',r)
        '\'' -> Just ('\'',r)
        '"' -> Just ('"',r)
        '?' -> Just ('\x3f',r)

        -- hexadecimals: \xhh... (variable length)
        'x' -> fmap (first chr) $ listToMaybe $ readP_to_S readHexP r

        -- hexadecimal unicode code points (below 10000): \uhhhh
        'u' | length r >= 4 -> case listToMaybe $ readP_to_S readHexP $ take 4 r of
            Just (n,[]) -> Just (chr n, drop 4 r)
            _ -> Nothing
        
        -- hexadecimal unicode code points: \Uhhhhhhhh
        'U' | length r >= 8 -> case listToMaybe $ readP_to_S readHexP $ take 8 r of
            Just (n,[]) -> Just (chr n, drop 8 r)
            _ -> Nothing

        -- octal numerals: \n or \nn or \nnn
        x | isOctDigit x -> case listToMaybe $ readP_to_S readOctP $ take 3 (x:r) of
            Just (n,t) -> Just (chr n, t ++ drop 3 (x:r))
            Nothing -> Nothing

        _ -> Nothing
    (c:r) -> Just (c,r)
    [] -> Nothing

structLiteral :: Parser Text
structLiteral = label "struct argument" $ do    
    char '{'
    str <- manyTill anySingle (char '}')
    return $ Text.pack $ '{' : str ++ "}"

arrayLiteral :: Parser Text
arrayLiteral = label "array argument" $ do
    char '['
    str <- manyTill anySingle (char ']')
    return $ Text.pack $ '[' : str ++ "]"

pid :: Parser PID
pid = PID <$> L.decimal

timestamp :: Parser Timestamp
timestamp = do
    s <- L.decimal
    "."
    ms <- L.decimal
    return $ MkSystemTime s (ms * 1000)

systemCallName :: Parser SystemCallName
systemCallName = SystemCallName <$> takeWhile1P (Just "system call name") (\s -> isAlphaNum s || s == '_')

signalName :: Parser SignalName
signalName = SignalName <$> takeWhile1P (Just "signal name") isAsciiUpper

lexeme :: Parser a -> Parser a
lexeme  = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

sc :: Parser ()
sc = L.space space1' empty empty

space1' :: Parser ()
space1' = void $ takeWhile1P (Just "white space") (== ' ')
