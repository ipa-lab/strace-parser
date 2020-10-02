{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Pass to parse system calls and signals and their arguments.
module Strace.Events (parseEvents) where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Strace.Parser
import Strace.Types
import Text.Megaparsec
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Char
import Data.Char
import Data.Functor

parseEvents :: Trace -> Trace
parseEvents = map $
  fmap $ \case
    SystemCall syscall -> SystemCall $ parseSystemCall syscall
    Signal signal -> Signal $ parseSignal signal
    x -> x

parseSystemCall :: SystemCall -> SystemCall
parseSystemCall c@(OtherSystemCall name args Finished) = case name of
  "execve" -> parse execve
  "openat" -> parse openat
  "close" -> parse close
  "read" -> parse read_
  "stat" -> parse stat
  _ -> c
  where
    parse f = fromMaybe c $ parseMaybe f args
parseSystemCall x = x

execve :: Parser SystemCall
execve = syscall3 Execve stringLiteral stringArray stringArray maybeErrno

eitherErrnoOr :: Parser a -> Parser (Either Errno a)
eitherErrnoOr p = (lexeme "-1" *> (Left <$> parseErrno) <* takeRest) <|> (Right <$> p)

maybeErrno :: Parser (Maybe Errno)
maybeErrno = either Just (const Nothing) <$> eitherErrnoOr "0"

syscall1 :: (a1 -> r -> SystemCall) -> Parser a1 -> Parser r -> Parser SystemCall
syscall1 f p1 pr = do
  "("
  a1 <- p1
  lexeme ")"
  lexeme "="
  r <- pr
  return $ f a1 r

syscall3 :: (a1 -> a2 -> a3 -> r -> SystemCall) -> Parser a1 -> Parser a2 -> Parser a3 -> Parser r -> Parser SystemCall
syscall3 f p1 p2 p3 pr = do
  "("
  a1 <- p1
  ", "
  a2 <- p2
  ", "
  a3 <- p3
  lexeme ")"
  lexeme "="
  r <- pr
  return $ f a1 a2 a3 r

openat :: Parser SystemCall
openat = syscall3 Openat pDirfd stringLiteral (parseFlags parseRead) (eitherErrnoOr fileDescriptor)

close :: Parser SystemCall
close = syscall1 Close fileDescriptor maybeErrno

read_ :: Parser SystemCall
read_ = syscall3 Read fileDescriptor stringLiteral L.decimal (eitherErrnoOr L.decimal)

stat :: Parser SystemCall
stat = do
  "("
  path <- stringLiteral
  ", "
  r <- try success <|> failure
  return $ Stat path r
  where
    success = do
      statStruct <- structLiteral
      lexeme ")"
      lexeme "="
      "0"
      return $ Right statStruct
    failure = do
      void $ "0x" *> L.hexadecimal
      lexeme ")"
      lexeme "="
      lexeme "-1"
      errno <- parseErrno
      void $ takeRest
      return $ Left errno

pDirfd :: Parser Dirfd
pDirfd = ("AT_FDCWD" *> return AT_FDCWD) <|> Dirfd <$> fileDescriptor

parseSignal :: Signal -> Signal
parseSignal s@(OtherSignal name info) = case name of
  _ -> s
parseSignal x = x

-- note: filenames are not considered strings and are never abbreviated
