{-# LANGUAGE LambdaCase #-}

-- | Pass to parse system calls and signals and their arguments.
module Strace.Events (parseEvents) where

import Strace.Types

parseEvents :: Trace -> Trace
parseEvents = map $
  fmap $ \case
    SystemCall syscall -> SystemCall $ parseSystemCall syscall
    Signal signal -> Signal $ parseSignal signal
    x -> x

parseSystemCall :: SystemCall -> SystemCall
parseSystemCall (OtherSystemCall name args Finished) = case name of
  _ -> undefined
parseSystemCall x = x

parseSignal :: Signal -> Signal
parseSignal (OtherSignal name info) = case name of
  _ -> undefined
parseSignal x = x