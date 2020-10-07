{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}

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
import Control.Applicative
import Text.ParserCombinators.ReadP (readP_to_S)
import Text.Read.Lex (readHexP, readOctP)
import Data.Attoparsec.Text
import Data.Attoparsec.Combinator
import Data.List

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
  fd <- signed decimal
  path <- optional $ Path <$> quotedString '<' '>'
  return $ FileDescriptor fd path

pDirfd :: Parser Dirfd
pDirfd = ("AT_FDCWD" *> return AT_FDCWD) <|> Dirfd <$> fileDescriptor

quotedString :: Char -> Char -> Parser Text
quotedString open close =
  Text.pack <$> (char open *> manyTill cLiteral (char close))

-- TODO: C string parsing could probably be vastly simplified
-- 1) we could assume hex-only strings (and always 1 byte per char i.e. \xhh)
-- 2) we could assume ASCII and use ByteStrings and push further decoding upstream
-- 3) maybe: use ByteStrings/Char8 and only look for escaped quote?

cLiteral :: Parser Char
cLiteral = do
  s <- lookAhead (count' 1 10 anyChar)
  case readCChar s of
    Just (c, r) -> c <$ Data.Attoparsec.Text.take (length s - length r)
    Nothing -> fail $ "unexpected: " ++ s

count' :: MonadPlus m => Int -> Int -> m a -> m [a]
count' m' n' p =
  if n' > 0 && n' >= m'
    then gom id m'
    else return []
  where
    gom f !m =
      if m > 0
        then do
          x <- p
          gom (f . (x:)) (m - 1)
        else god f (if m' <= 0 then n' else n' - m')
    god f !d =
      if d > 0
        then do
          r <- optional p
          case r of
            Nothing -> return (f [])
            Just  x -> god (f . (x:)) (d - 1)
        else return (f [])
{-# INLINE count' #-}

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
    'u' | length r >= 4 -> case listToMaybe $ readP_to_S readHexP $ Data.List.take 4 r of
      Just (n, []) -> Just (chr n, drop 4 r)
      _ -> Nothing
    -- hexadecimal unicode code points: \Uhhhhhhhh
    'U' | length r >= 8 -> case listToMaybe $ readP_to_S readHexP $ Data.List.take 8 r of
      Just (n, []) -> Just (chr n, drop 8 r)
      _ -> Nothing
    -- octal numerals: \n or \nn or \nnn
    x | isOctDigit x -> case listToMaybe $ readP_to_S readOctP $ Data.List.take 3 (x : r) of
      Just (n, t) -> Just (chr n, t ++ drop 3 (x : r))
      Nothing -> Nothing
    _ -> Nothing
  (c : r) -> Just (c, r)
  [] -> Nothing

arrayOf :: Parser a -> Parser [a]
arrayOf p = char '[' *> p `sepBy` ", " <* char ']'

pointerTo :: Parser a -> Parser (Pointer a)
pointerTo p = (Pointer <$> ("0x" *> hexadecimal)) <|> (Deref <$> p)

parseErrno :: Parser Errno
parseErrno = Errno <$> takeWhile1 isAsciiUpper

parseFlags :: Parser Flags
parseFlags = Set.fromList <$> takeWhile1 (\s -> isAlphaNum s || s == '_') `sepBy` char '|'

-- TODO: returns just a string
-- TODO: can't handle nested structs
struct :: Parser Text
struct = do
  char '{'
  str <- takeTill (== '}')
  char '}'
  return $ '{' `Text.cons` str `Text.snoc` '}'

pid :: Parser ProcessID
pid = decimal

timestamp :: Parser SystemTime
timestamp = do
  s <- decimal
  "."
  ms <- decimal
  return $ MkSystemTime s (ms * 1000)

systemCallName :: Parser SystemCallName
systemCallName = SystemCallName <$> takeWhile1 (\s -> isAlphaNum s || s == '_')

signalName :: Parser SignalName
signalName = SignalName <$> takeWhile1 isAsciiUpper

skipHorizontalSpace :: Parser ()
skipHorizontalSpace = skipWhile isHorizontalSpace
