{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Common parsing types and functions.
module Strace.Parser where

import Control.Monad
import Data.Bifunctor
import Data.Char
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock.System
import Data.Void
import Strace.Types
import System.Posix.Types
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Text.ParserCombinators.ReadP (ReadP, readP_to_S)
import Text.Read.Lex (readHexP, readOctP)

type Parser = Parsec Void Text

str :: Parser Str
str = do
  str <- quotedString '"' '"'
  ellipsis <- optional "..."
  if isJust ellipsis
    then return $ Truncated str
    else return $ Complete str

path :: Parser Path
path = Path <$> quotedString '"' '"'

fileDescriptor :: Parser FileDescriptor
fileDescriptor = do
  fd <- L.signed (pure ()) L.decimal
  path <- optional $ Path <$> quotedString '<' '>'
  return $ FileDescriptor fd path

pDirfd :: Parser Dirfd
pDirfd = ("AT_FDCWD" *> return AT_FDCWD) <|> Dirfd <$> fileDescriptor

quotedString :: Char -> Char -> Parser Text
quotedString open close = label "string" $ 
  Text.pack <$> (char open *> manyTill cLiteral (char close))

cLiteral :: Parser Char
cLiteral = do
  s <- lookAhead (count' 1 10 anySingle)
  case readCChar s of
    Just (c, r) -> c <$ skipCount (length s - length r) anySingle
    Nothing -> unexpected (Tokens (head s :| []))

readCChar :: String -> Maybe (Char, String)
readCChar s = case s of
  ('\\' : c : r) -> case c of
    'a' -> Just ('\a', r)
    'b' -> Just ('\b', r)
    'e' -> Just ('\x1b', r)
    'f' -> Just ('\f', r)
    'n' -> Just ('\n', r)
    'r' -> Just ('\r', r)
    't' -> Just ('\t', r)
    'v' -> Just ('\v', r)
    '\\' -> Just ('\\', r)
    '\'' -> Just ('\'', r)
    '"' -> Just ('"', r)
    '?' -> Just ('\x3f', r)
    -- hexadecimals: \xhh... (variable length)
    'x' -> fmap (first chr) $ listToMaybe $ readP_to_S readHexP r
    -- hexadecimal unicode code points (below 10000): \uhhhh
    'u' | length r >= 4 -> case listToMaybe $ readP_to_S readHexP $ take 4 r of
      Just (n, []) -> Just (chr n, drop 4 r)
      _ -> Nothing
    -- hexadecimal unicode code points: \Uhhhhhhhh
    'U' | length r >= 8 -> case listToMaybe $ readP_to_S readHexP $ take 8 r of
      Just (n, []) -> Just (chr n, drop 8 r)
      _ -> Nothing
    -- octal numerals: \n or \nn or \nnn
    x | isOctDigit x -> case listToMaybe $ readP_to_S readOctP $ take 3 (x : r) of
      Just (n, t) -> Just (chr n, t ++ drop 3 (x : r))
      Nothing -> Nothing
    _ -> Nothing
  (c : r) -> Just (c, r)
  [] -> Nothing

arrayOf :: Parser a -> Parser [a]
arrayOf p = char '[' *> p `sepBy` ", " <* char ']'

pointerTo :: Parser a -> Parser (Pointer a)
pointerTo p = (Pointer <$> ("0x" *> L.hexadecimal)) <|> (Deref <$> p)

parseErrno :: Parser Errno
parseErrno = Errno <$> takeWhile1P Nothing isAsciiUpper

parseFlags :: Parser Flags
parseFlags = Set.fromList <$> takeWhile1P Nothing (\s -> isAlphaNum s || s == '_') `sepBy` char '|'

-- TODO: returns just a string
-- TODO: can't handle nested structs
struct :: Parser Text
struct = label "struct argument" $ do
  char '{'
  str <- manyTill anySingle (char '}')
  return $ Text.pack $ '{' : str ++ "}"

pid :: Parser ProcessID
pid = L.decimal

timestamp :: Parser SystemTime
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
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

sc :: Parser ()
sc = L.space space1' empty empty

space1' :: Parser ()
space1' = void $ takeWhile1P (Just "white space") (== ' ')
