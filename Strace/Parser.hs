{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Common parsing types and functions.
module Strace.Parser where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString.Char8 hiding (char8)
import Data.ByteString.Builder
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy.Char8 qualified as L
import Data.Char
import Data.Maybe
import Data.Set qualified as Set
import Data.Time.Clock.System
import Strace.Types
import System.Posix.Types
import Prelude hiding (takeWhile)

str :: Parser Str
str = do
  str <- quotedString
  ellipsis <- optional "..."
  if isJust ellipsis
    then return $ Truncated str
    else return $ Complete str

path :: Parser Path
path = Path <$> quotedString

fileDescriptor :: Parser FileDescriptor
fileDescriptor = do
  fd <- signed decimal
  path <- optional $ Path <$> (char '<' *> takeWhile (/= '>') <* char '>')
  return $ FileDescriptor fd path

pDirfd :: Parser Dirfd
pDirfd = ("AT_FDCWD" *> return AT_FDCWD) <|> Dirfd <$> fileDescriptor

quotedString :: Parser ByteString
quotedString = toBS <$> quotedStringBuilder

toBS :: Builder -> ByteString
toBS = BS.concat . L.toChunks . toLazyByteString

quotedStringBuilder :: Parser Builder
quotedStringBuilder = char '"' *> go mempty <* char '"'
  where
    go t0 = do
      t1 <- takeWhile (/= '"')
      if endsWithOddNumberOfSlashes t1
        then char '"' *> go (t0 <> byteString t1 <> char8 '"')
        else return (t0 <> byteString t1)

endsWithOddNumberOfSlashes :: ByteString -> Bool
endsWithOddNumberOfSlashes t = go False (BS.length t - 1)
  where
    go !b !n
      | n < 0 = b
      | BS.index t n == '\\' = go (not b) (n -1)
      | otherwise = b

arrayOf :: Parser a -> Parser [a]
arrayOf p = char '[' *> p `sepBy` ", " <* char ']'

pointerTo :: Parser a -> Parser (Pointer a)
pointerTo p =
  choice
    [ Pointer <$> ("0x" *> hexadecimal),
      Pointer <$> ("NULL" *> pure 0),
      Deref <$> p
    ]

parseErrno :: Parser Errno
parseErrno = Errno <$> takeWhile1 isAsciiUpper

parseFlags :: Parser Flags
parseFlags = Flags <$> Set.fromList <$> takeWhile1 (\s -> isAlphaNum s || s == '_') `sepBy` char '|'

struct :: Parser Struct
struct = Struct <$> toBS <$> structBuilder

structBuilder :: Parser Builder
structBuilder = char '{' *> go (char8 '{') <* char '}'
  where
    go t0 = do
      t1 <- byteString <$> takeWhile (\c -> c /= '"' && c /= '{' && c /= '}')
      c1 <- peekChar'
      case c1 of
        '"' -> do
          t2 <- quotedStringBuilder
          go (t0 <> t1 <> char8 '"' <> t2 <> char8 '"')
        '{' -> do
          t2 <- structBuilder
          go (t0 <> t1 <> t2)
        '}' -> return (t0 <> t1 <> char8 '}')
        _ -> fail "impossible"

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
signalName = SignalName <$> takeWhile1 (\s -> isAlphaNum s || s == '_')

skipHorizontalSpace :: Parser ()
skipHorizontalSpace = skipWhile (\c -> c == ' ' || c == '\t')
