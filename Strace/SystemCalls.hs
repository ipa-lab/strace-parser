{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Pass to parse system calls and signals and their arguments.
module Strace.SystemCalls (parseEvents, parseSystemCall) where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 qualified as BS
import Debug.Trace
import Strace.Parser
import Strace.Types

parseEvents :: Trace -> Trace
parseEvents = map $ mapEvent $ mapSystemCall parseSystemCall

parseSystemCall :: SystemCall -> SystemCall
parseSystemCall c@(OtherSystemCall (SystemCallName name) args Finished) = case name of
  "close" -> parse $ Close <$> call1 MkClose fileDescriptor maybeErrno
  "connect" -> parse $ Connect <$> call3 MkConnect fileDescriptor (pointerTo struct) decimal maybeErrno
  "dup" -> parse $ Dup <$> call1 MkDup fileDescriptor (eitherErrnoOr fileDescriptor)
  "dup2" -> parse $ Dup2 <$> call2 MkDup2 fileDescriptor fileDescriptor (eitherErrnoOr fileDescriptor)
  "dup3" -> parse $ Dup3 <$> call3 MkDup3 fileDescriptor fileDescriptor parseFlags (eitherErrnoOr fileDescriptor)
  "execve" -> parse $ Execve <$> call3 MkExecve (pointerTo path) (pointerTo (arrayOf str)) (pointerTo (arrayOf str)) maybeErrno
  "fstat" -> parse $ Fstat <$> call2 MkFstat fileDescriptor (pointerTo struct) maybeErrno
  "fstatat" -> parse $ Fstatat <$> call4 MkFstatat pDirfd (pointerTo path) (pointerTo struct) parseFlags maybeErrno
  "fstatfs" -> parse $ Fstatfs <$> call2 MkFstatfs fileDescriptor (pointerTo struct) maybeErrno
  "lstat" -> parse $ Lstat <$> call2 MkLstat (pointerTo path) (pointerTo struct) maybeErrno
  "openat" -> parse $ Openat <$> call3'4 MkOpenat pDirfd (pointerTo path) parseFlags parseFlags (eitherErrnoOr fileDescriptor)
  "pipe" -> parse $ Pipe <$> call1 MkPipe (pointerTo (arrayOf fileDescriptor)) maybeErrno
  "read" -> parse $ Read <$> call3 MkRead fileDescriptor (pointerTo str) decimal (eitherErrnoOr decimal)
  "rmdir" -> parse $ Rmdir <$> call1 MkRmdir (pointerTo path) maybeErrno
  "stat" -> parse $ Stat <$> call2 MkStat (pointerTo path) (pointerTo struct) maybeErrno
  "statfs" -> parse $ Statfs <$> call2 MkStatfs (pointerTo path) (pointerTo struct) maybeErrno
  "write" -> parse $ Write <$> call3 MkWrite fileDescriptor (pointerTo str) decimal (eitherErrnoOr decimal)
  _ -> c
  where
    --parse f = fromMaybe c $ parseMaybe f args
    parse f = case parseOnly f args of
      Left err -> trace ("error parsing " ++ BS.unpack name ++ ": " ++ err) c
      Right x -> x
parseSystemCall x = x

eitherErrnoOr :: Parser a -> Parser (Either Errno a)
eitherErrnoOr p =
  ("-1" *> skipHorizontalSpace *> (Left <$> parseErrno) <* takeByteString) <|> (Right <$> p)

maybeErrno :: Parser (Maybe Errno)
maybeErrno = either Just (const Nothing) <$> eitherErrnoOr "0"

call1 ::
  (a1 -> r -> b) ->
  Parser a1 ->
  Parser r ->
  Parser b
call1 f p1 pr = do
  a1 <- "(" *> p1
  r <- ")" *> skipHorizontalSpace *> "=" *> skipHorizontalSpace *> pr
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
  r <- ")" *> skipHorizontalSpace *> "=" *> skipHorizontalSpace *> pr
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
  r <- ")" *> skipHorizontalSpace *> "=" *> skipHorizontalSpace *> pr
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
  r <- ")" *> skipHorizontalSpace *> "=" *> skipHorizontalSpace *> pr
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
  r <- ")" *> skipHorizontalSpace *> "=" *> skipHorizontalSpace *> pr
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
  r <- ")" *> skipHorizontalSpace *> "=" *> skipHorizontalSpace *> pr
  return $ f a1 a2 a3 a4 r
