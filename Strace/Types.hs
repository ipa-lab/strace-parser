{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StrictData #-}

module Strace.Types where

import Data.Text (Text)
import Data.Time.Clock.System (SystemTime)
import Data.Word (Word32)

-- | A system trace.
type Trace = [Line Event]

-- | A trace line, approximately.
--
-- Immediately after parsing, a `Line` corresponds directly to an @strace@ log
-- line. After putting interrupted system calls back together, a `Line` may
-- represent a system event that is actually derived from multiple lines in the
-- original @strace@ log.
data Line a = Line (Maybe PID) (Maybe Time) a
  deriving (Show, Functor)

-- | A system event.
data Event
  = SystemCall SystemCall
  | Signal Signal
  | Killed SignalName
  | Exit Int
  deriving (Show)

data SystemCall
  = OtherSystemCall SystemCallName Text SystemCallStatus
  deriving (Show)

-- | Indicates whether the system call was interrupted.
data SystemCallStatus = Finished | Unfinished | Resumed
  deriving (Eq, Ord, Show)

data Signal
  = OtherSignal SignalName Text
  deriving (Show)

-- | A process ID.
newtype PID = PID Word32
  deriving (Num, Enum, Eq, Ord, Show)

data Time = Instant Timestamp | Duration Timestamp Timestamp
  deriving (Show)

type Timestamp = SystemTime

newtype SystemCallName = SystemCallName Text
  deriving (Eq, Ord, Show)

newtype SignalName = SignalName Text
  deriving (Eq, Ord, Show)