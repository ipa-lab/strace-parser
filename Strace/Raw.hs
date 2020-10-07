{-# LANGUAGE OverloadedStrings #-}

-- | This module contains functions to parse "raw" @strace@ files.
module Strace.Raw (parseRawTrace) where

import Control.Monad
import Data.Bifunctor
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Strace.Parser
import Strace.Types
import Data.Attoparsec.Text
import Data.Attoparsec.Combinator
import Control.Applicative
import Prelude hiding (takeWhile)

parseRawTrace :: FilePath -> IO (Either String Trace)
parseRawTrace file = parseOnly trace <$> Text.readFile file

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
  info <- Text.pack <$> manyTill anyChar " ---"
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
  let syscall = case Text.stripSuffix " <unfinished ...>" line of
        Nothing -> OtherSystemCall name line Finished
        Just line' -> OtherSystemCall name line' Unfinished
  return $ SystemCall syscall
