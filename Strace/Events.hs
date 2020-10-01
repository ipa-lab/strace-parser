{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Pass to parse system calls and signals and their arguments.
module Strace.Events (parseEvents) where

import Strace.Types
import Strace.Parser
import Data.Maybe
import Control.Applicative

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
  _ -> c
 where
   parse f = fromMaybe c $ parseMaybe f args

parseSystemCall x = x

execve :: Parser SystemCall
execve = do
  "("
  path <- stringLiteral
  ", "
  args <- stringArray
  ", "
  env <- stringArray
  lexeme ")"
  retval <- returnValue
  return $ Execve path args env retval

openat :: Parser SystemCall
openat = do
  "("
  dirfd <- pDirfd
  ", "
  path <- stringLiteral
  ", "
  flags <- parseFlags parseRead
  lexeme ")"
  fd <- returnValue -- TODO
  return $ Openat dirfd path flags fd

close :: Parser SystemCall
close = do
  "("
  fd <- fileDescriptor
  lexeme ")"
  retval <- returnValue
  return $ Close fd retval  

pDirfd :: Parser Dirfd
pDirfd = ("AT_FDCWD" *> return AT_FDCWD) <|> Dirfd <$> fileDescriptor

parseSignal :: Signal -> Signal
parseSignal s@(OtherSignal name info) = case name of
  _ -> s
parseSignal x = x


-- note: filenames are not considered strings and are never abbreviated



