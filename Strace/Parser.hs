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
quotedString = char '"' *> (BS.concat . L.toChunks . toLazyByteString <$> go mempty) <* char '"'
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
pointerTo p = (Pointer <$> ("0x" *> hexadecimal)) <|> (Deref <$> p)

parseErrno :: Parser Errno
parseErrno = Errno <$> takeWhile1 isAsciiUpper

parseFlags :: Parser Flags
parseFlags = Set.fromList <$> takeWhile1 (\s -> isAlphaNum s || s == '_') `sepBy` char '|'

struct :: Parser ByteString
struct = do
  char '{'
  str <- go
  return $ '{' `BS.cons` str
  where
    go = do
      str1 <- takeWhile (\c -> c /= '"' && c /= '{' && c /= '}')
      c1 <- peekChar'
      case c1 of
        '"' -> do
          str2 <- quotedString
          str3 <- go
          return $ str1 <> (('"' `BS.cons` str2) `BS.snoc` '"') <> str3
        '{' -> do
          str2 <- struct
          str3 <- go
          return $ str1 <> str2 <> str3
        '}' -> do
          char '}'
          return $ str1 `BS.snoc` '}'
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
signalName = SignalName <$> takeWhile1 isAsciiUpper

skipHorizontalSpace :: Parser ()
skipHorizontalSpace = skipWhile (\c -> c == ' ' || c == '\t')
