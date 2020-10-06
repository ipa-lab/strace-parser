{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Pass to parse system calls and signals and their arguments.
module Strace.SystemCalls (parseEvents) where

import Control.Applicative
import Debug.Trace
import Strace.Parser
import Strace.Types
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Data.Text qualified as Text
import Data.Maybe

parseEvents :: Trace -> Trace
parseEvents = map $ mapEvent $ mapSystemCall parseSystemCall

parseSystemCall :: SystemCall -> SystemCall
parseSystemCall c@(OtherSystemCall (SystemCallName name) args Finished) = case name of
  "close" -> parse $ Close <$> call1 MkClose fileDescriptor maybeErrno
  "dup" -> parse $ Dup <$> call1 MkDup fileDescriptor (eitherErrnoOr fileDescriptor)
  "dup2" -> parse $ Dup2 <$> call2 MkDup2 fileDescriptor fileDescriptor (eitherErrnoOr fileDescriptor)
  "dup3" -> parse $ Dup3 <$> call3 MkDup3 fileDescriptor fileDescriptor parseFlags (eitherErrnoOr fileDescriptor)
  "execve" -> parse $ Execve <$> call3 MkExecve (pointer stringLiteral) (pointer $ arrayLiteral stringLiteral) (pointer $ arrayLiteral stringLiteral) maybeErrno
  "fstat" -> parse $ Fstat <$> call2 MkFstat fileDescriptor (pointer structLiteral) maybeErrno
  "fstatat" -> parse $ Fstatat <$> call4 MkFstatat pDirfd (pointer stringLiteral) (pointer structLiteral) parseFlags maybeErrno
  "lstat" -> parse $ Lstat <$> call2 MkLstat (pointer stringLiteral) (pointer structLiteral) maybeErrno
  "openat" -> parse $ Openat <$> call3'4 MkOpenat pDirfd (pointer stringLiteral) parseFlags parseFlags (eitherErrnoOr fileDescriptor)
  "pipe" -> parse $ Pipe <$> call1 MkPipe (pointer $ arrayLiteral fileDescriptor) maybeErrno
  "read" -> parse $ Read <$> call3 MkRead fileDescriptor (pointer stringLiteral) L.decimal (eitherErrnoOr L.decimal)
  "rmdir" -> parse $ Rmdir <$> call1 MkRmdir (pointer stringLiteral) maybeErrno
  "stat" -> parse $ Stat <$> call2 MkStat (pointer stringLiteral) (pointer structLiteral) maybeErrno
  "write" -> parse $ Write <$> call3 MkWrite fileDescriptor (pointer stringLiteral) L.decimal (eitherErrnoOr L.decimal)
  _ -> c
  where
    --parse f = fromMaybe c $ parseMaybe f args
    parse f = case Text.Megaparsec.parse f "" args of
      Left err -> trace ("error parsing " ++ Text.unpack name ++ ": " ++ errorBundlePretty err) c
      Right x -> x
parseSystemCall x = x

eitherErrnoOr :: Parser a -> Parser (Either Errno a)
eitherErrnoOr p = (lexeme "-1" *> (Left <$> parseErrno) <* takeRest) <|> (Right <$> p)

maybeErrno :: Parser (Maybe Errno)
maybeErrno = either Just (const Nothing) <$> eitherErrnoOr "0"

call1 ::
  (a1 -> r -> b) ->
  Parser a1 ->
  Parser r ->
  Parser b
call1 f p1 pr = do
  a1 <- "(" *> p1
  r <- ")" *> hspace1 *> "=" *> hspace1 *> pr
  return $ f a1 r

call2 ::
  (a1 -> a2 -> r -> b) ->
  Parser a1 ->
  Parser a2 ->
  Parser r ->
  Parser b
call2 f p1 p2 pr = do
  a1 <- "(" *> p1
  a2 <- ", " *> p2
  r <- ")" *> hspace1 *> "=" *> hspace1 *> pr
  return $ f a1 a2 r

call2'3 ::
  (a1 -> a2 -> Maybe a3 -> r -> b) ->
  Parser a1 ->
  Parser a2 ->
  Parser a3 ->
  Parser r ->
  Parser b
call2'3 f p1 p2 p3 pr = do
  a1 <- "(" *> p1
  a2 <- ", " *> p2
  a3 <- optional $ ", " *> p3
  r <- ")" *> hspace1 *> "=" *> hspace1 *> pr
  return $ f a1 a2 a3 r

call3 ::
  (a1 -> a2 -> a3 -> r -> b) ->
  Parser a1 ->
  Parser a2 ->
  Parser a3 ->
  Parser r ->
  Parser b
call3 f p1 p2 p3 pr = do
  a1 <- "(" *> p1
  a2 <- ", " *> p2
  a3 <- ", " *> p3
  r <- ")" *> hspace1 *> "=" *> hspace1 *> pr
  return $ f a1 a2 a3 r

call3'4 ::
  (a1 -> a2 -> a3 -> (Maybe a4) -> r -> b) ->
  Parser a1 ->
  Parser a2 ->
  Parser a3 ->
  Parser a4 ->
  Parser r ->
  Parser b
call3'4 f p1 p2 p3 p4 pr = do
  a1 <- "(" *> p1
  a2 <- ", " *> p2
  a3 <- ", " *> p3
  a4 <- optional $ ", " *> p4
  r <- ")" *> hspace1 *> "=" *> hspace1 *> pr
  return $ f a1 a2 a3 a4 r

call4 ::
  (a1 -> a2 -> a3 -> a4 -> r -> b) ->
  Parser a1 ->
  Parser a2 ->
  Parser a3 ->
  Parser a4 ->
  Parser r ->
  Parser b
call4 f p1 p2 p3 p4 pr = do
  a1 <- "(" *> p1
  a2 <- ", " *> p2
  a3 <- ", " *> p3
  a4 <- ", " *> p4
  r <- ")" *> hspace1 *> "=" *> hspace1 *> pr
  return $ f a1 a2 a3 a4 r

pDirfd :: Parser Dirfd
pDirfd = ("AT_FDCWD" *> return AT_FDCWD) <|> Dirfd <$> fileDescriptor
