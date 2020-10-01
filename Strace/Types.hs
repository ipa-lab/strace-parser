{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

module Strace.Types where

import Data.String (IsString)
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
data Line a = Line PID SystemTime a
  deriving (Show, Functor)

-- | A system event.
data Event
  = SystemCall SystemCall
  | Signal Signal
  | Killed SignalName
  | Exit Int
  deriving (Show)

data SystemCall
  = Openat Dirfd Path (Flags OpenFlag) FileDescriptor  -- TODO: mode, errors
  | Close FileDescriptor Int
  | Execve Path [Text] [Text] Int
  | OtherSystemCall SystemCallName Text SystemCallStatus
  deriving (Show)

type Path = Text

type FileDescriptor = Int  -- TODO

data Dirfd = AT_FDCWD | Dirfd FileDescriptor
  deriving (Eq, Ord, Show)

type Flags a = [a] -- TODO: more efficient representation (set/bitset/...)

data OpenFlag
  = O_RDONLY
  | O_WRONLY
  | O_RDWR
  | O_APPEND
  | O_ASYNC
  | O_CLOEXEC
  | O_CREAT
  | O_DIRECT
  | O_DIRECTORY
  | O_DSYNC
  | O_EXCL
  | O_LARGEFILE
  | O_NOATIME
  | O_NOCTTY
  | O_NOFOLLOW
  | O_NONBLOCK
  | O_NDELAY
  | O_PATH
  | O_SYNC
  | O_TMPFILE
  | O_TRUNC
  deriving (Eq, Ord, Read, Show)

-- | Indicates whether the system call was interrupted.
data SystemCallStatus = Finished | Unfinished | Resumed
  deriving (Eq, Ord, Show)

data Signal
  = OtherSignal SignalName Text
  deriving (Show)

-- | A process ID.
newtype PID = PID Word32
  deriving (Num, Enum, Eq, Ord, Show)

newtype SystemCallName = SystemCallName Text
  deriving (Eq, Ord, Show, IsString)

newtype SignalName = SignalName Text
  deriving (Eq, Ord, Show, IsString)
