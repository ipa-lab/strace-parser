{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Common parsing types and functions.
module Strace.Parser where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text
import Data.Char
import Data.Maybe
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
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

quotedString :: Parser Text
quotedString = do
  char '"'
  go
  where
    go = do
      str <- takeWhile (\c -> c /= '\\' && c /= '"')
      c1 <- anyChar
      if c1 == '"'
        then return str
        else do
          c2 <- anyChar
          let str' = if c2 == '"' then str `Text.snoc` c2 else str `Text.snoc` c1 `Text.snoc` c2
          liftA2 Text.append (return str') go

arrayOf :: Parser a -> Parser [a]
arrayOf p = char '[' *> p `sepBy` ", " <* char ']'

pointerTo :: Parser a -> Parser (Pointer a)
pointerTo p = (Pointer <$> ("0x" *> hexadecimal)) <|> (Deref <$> p)

parseErrno :: Parser Errno
parseErrno = Errno <$> takeWhile1 isAsciiUpper

parseFlags :: Parser Flags
parseFlags = Set.fromList <$> takeWhile1 (\s -> isAlphaNum s || s == '_') `sepBy` char '|'

struct :: Parser Text
struct = do
  char '{'
  str <- go
  return $ '{' `Text.cons` str
  where
    go = do
      str1 <- takeWhile (\c -> c /= '"' && c /= '{' && c /= '}')
      c1 <- peekChar'
      case c1 of
        '"' -> do
          str2 <- quotedString
          str3 <- go
          return $ str1 <> ('"' `Text.cons` str2 `Text.snoc` '"') <> str3
        '{' -> do          
          str2 <- struct
          str3 <- go
          return $ str1 <> str2 <> str3
        '}' -> do
          char '}'
          return $ str1 `Text.snoc` '}'
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
skipHorizontalSpace = skipWhile isHorizontalSpace
