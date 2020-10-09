{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Strace.SystemCalls
import Strace.Finish
import Strace.Raw
import Strace.Types
import System.Environment
import Text.Pretty.Simple (pPrint)
import Text.Printf
import System.IO
import Data.Map (Map)
import Data.Map qualified as Map
import Data.List
import Control.Monad
import Data.Text (Text)
import Data.Text qualified as Text
import System.Clock

-- We assume the following strace invocation:
--
--      strace -f -ttt -v -x -y -s 1048576
--
-- This follows forks and shows PIDs (-f), prints timestamps in the right format
-- (-ttt), expands all structures incl. env vars (-v), prints non-ascii strings
-- in hex format (-x), resolves file descriptors (-y) and prints strings up to 1
-- MB (-s 1048576).
--
-- TODO: check whether -x is necessary

main :: IO ()
main = do
  [file] <- getArgs

  hSetBuffering stdout NoBuffering

  printf "Parsing %s ... " file
  !r1 <- parseRawTrace file
  case r1 of
    Left err -> printf "\n%s\n" err
    Right t1 -> do
      printf "%d lines\n" (length t1)
      
      printf "Unbreaking system calls ... "
      let !t2 = finishSystemCalls t1
      printf "%d events\n" (length t2)

      printf "Parsing events ... "
      start <- getTime Monotonic
      let !t3 = parseEvents t2

      let isUnknown (Line _ _ (SystemCall (OtherSystemCall _ _ _))) = True
          isUnknown _ = False
      let numUnknown = length $ filter isUnknown t3
      let numKnown = length t3 - numUnknown

      printf "%d known / %d unknown " numKnown numUnknown

      end <- getTime Monotonic
      let time :: Double = (fromIntegral $ toNanoSecs (diffTimeSpec start end)) / 1e9
      printf "(%f s)\n" time

--      pPrint $ take 100 t3

      --let counts = countEvents t3
      --forM_ (sortOn snd $ Map.toList counts) $ \(n,c) -> printf "%20s\t%d\n" (Text.unpack n) c

countEvents :: Trace -> Map Text Int
countEvents = go Map.empty
  where
    go m ((Line _ _ (SystemCall (OtherSystemCall (SystemCallName name) _ _))):ls) = 
      let m' = Map.alter incr name m in go m' ls
    go m (_:ls) = go m ls
    go m [] = m
  
    incr Nothing = Just 1
    incr (Just a) = Just (a + 1)

