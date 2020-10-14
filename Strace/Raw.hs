{-# LANGUAGE OverloadedStrings #-}

-- | This module contains functions to parse "raw" @strace@ files.
module Strace.Raw (parseRawTrace, trace, line) where

import Control.Monad
import Strace.Parser
import Strace.Types
import Data.Attoparsec.ByteString.Char8
import Control.Applicative
import Prelude hiding (takeWhile)
import qualified Data.ByteString.Char8 as BS

parseRawTrace :: FilePath -> IO (Either String Trace)
parseRawTrace file = parseOnly trace <$> BS.readFile file

trace :: Parser Trace
trace = line `manyTill` endOfInput

line :: Parser Line
line = do
  pid <- pid
  skipHorizontalSpace
  timestamp <- timestamp
  skipHorizontalSpace
  event <- choice [exit, killed, signal, systemCallResumed, systemCall]
  endOfLine <|> endOfInput
  return $ Line pid timestamp event

exit :: Parser Event
exit = Exit <$> ("+++ exited with " *> decimal <* " +++")

killed :: Parser Event
killed = Killed <$> ("+++ killed by " *> signalName <* " +++")

signal :: Parser Event
signal = do
  "--- "
  name <- signalName
  skipHorizontalSpace
  info <- BS.pack <$> manyTill anyChar " ---"
  return $ Signal name info

systemCallResumed :: Parser Event
systemCallResumed = do
  "<... "
  name <- systemCallName
  " resumed>"
  line <- takeWhile (/= '\n')
  return $ SystemCall $ OtherSystemCall name line Resumed

systemCall :: Parser Event
systemCall = do
  name <- systemCallName
  skipHorizontalSpace
  line <- takeWhile (/= '\n')
  let syscall = case BS.stripSuffix " <unfinished ...>" line of
        Nothing -> OtherSystemCall name line Finished
        Just line' -> OtherSystemCall name line' Unfinished
  return $ SystemCall syscall
