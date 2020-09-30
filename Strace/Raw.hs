{-# LANGUAGE OverloadedStrings #-}

-- | This module contains functions to parse "raw" @strace@ files.
module Strace.Raw (parseRawTrace) where

import Control.Monad
import Data.Bifunctor
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Strace.Parser
import Strace.Types
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

parseRawTrace :: FilePath -> IO (Either String Trace)
parseRawTrace file =
  first errorBundlePretty . parse (line `manyTill` eof) file <$> Text.readFile file

line :: Parser (Line Event)
line = do
  pid <- lexeme pid
  timestamp <- lexeme timestamp
  event <- choice [exit, killed, signal, systemCallResumed, systemCall]
  void eol <|> eof
  return $ Line (Just pid) (Just $ Instant timestamp) event -- TODO

exit :: Parser Event
exit = Exit <$> (symbol "+++ exited with" *> lexeme L.decimal <* symbol "+++")

killed :: Parser Event
killed = Killed <$> (symbol "+++ killed by" *> lexeme signalName <* symbol "+++")

signal :: Parser Event
signal = do
  symbol "---"
  name <- lexeme signalName
  info <- Text.pack <$> manyTill anySingle " ---" <?> "siginfo"
  return $ Signal $ OtherSignal name info

systemCallResumed :: Parser Event
systemCallResumed = do
  symbol "<..."
  name <- lexeme systemCallName
  symbol "resumed>"
  line <- takeWhileP Nothing (/= '\n')
  return $ SystemCall $ OtherSystemCall name line Resumed

systemCall :: Parser Event
systemCall = do
  name <- lexeme systemCallName
  line <- takeWhileP Nothing (/= '\n')
  let syscall = case Text.stripSuffix " <unfinished ...>" line of
        Nothing -> OtherSystemCall name line Finished
        Just line' -> OtherSystemCall name line' Unfinished
  return $ SystemCall syscall
