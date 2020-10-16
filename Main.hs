{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad
import Control.Monad.Trans.Resource
import Data.Attoparsec.ByteString.Streaming qualified as AS
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Streaming.Char8 qualified as Q
import Data.Function
import Data.List
import Data.Map (Map)
import Data.Map qualified as Map
import Strace.Finish
import Strace.Raw
import Strace.SystemCalls
import Strace.Types
import Streaming
import Streaming.Prelude qualified as S
import System.Clock
import System.Environment
import System.IO
import Text.Pretty.Simple (pPrint)
import Text.Printf

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

  start <- getTime Monotonic

  r <-
    runResourceT $
      Q.readFile file
        & AS.parsed line
        & finishSystemCalls
        & S.map (mapEvent $ mapSystemCall parseSystemCall)
        & (S.fold_ (\(i :: Int) _ -> i + 1) 0 id)
--        & (S.fold_ countUnknownSysCalls mempty id)

  print r
--  pPrint $ sortBy (compare `on` snd) $ Map.toList r

  end <- getTime Monotonic
  let time :: Double = (fromIntegral $ toNanoSecs (diffTimeSpec start end)) / 1e9
  printf "(%f s)\n" time



countUnknownSysCalls :: Map SystemCallName Int -> Line -> Map SystemCallName Int
countUnknownSysCalls m (Line _ _ (SystemCall (OtherSystemCall name _ _))) = 
  Map.insertWith (+) name 1 m
countUnknownSysCalls m _ = m
