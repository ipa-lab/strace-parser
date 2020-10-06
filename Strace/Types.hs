{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

module Strace.Types where

import Data.Set (Set)
import Data.String (IsString)
import Data.Text (Text)
import Data.Time.Clock.System (SystemTime)
import System.Posix.Types

-- | A system trace, which is simply a list of trace lines.
type Trace = [Line]

-- | A trace line.
--
-- Initially, a `Line` corresponds directly to an @strace@ log line. After
-- unbreaking interrupted system calls, this is no longer the case, and a `Line`
-- may represent a system event that is derived from multiple lines in the
-- original @strace@ log.
data Line = Line ProcessID SystemTime Event
  deriving (Show)

mapEvent :: (Event -> Event) -> Line -> Line
mapEvent f (Line pid t ev) = Line pid t (f ev)

-- | A system event.
data Event
  = SystemCall SystemCall
  | Signal SignalName SigInfo
  | Killed SignalName
  | Exit Int
  deriving (Show)

mapSystemCall :: (SystemCall -> SystemCall) -> Event -> Event
mapSystemCall f (SystemCall sc) = SystemCall (f sc)
mapSystemCall _ ev = ev

-- | A Linux system call.
--
-- See <https://man7.org/linux/man-pages/man2/syscalls.2.html>.
data SystemCall
  = Openat Openat
  | Close Close
  | Read Read_
  | Execve Execve
  | Stat Path (Maybe StatStruct) (Maybe Errno)
  | OtherSystemCall SystemCallName Text SystemCallStatus
  deriving (Show)

data Openat = MkOpenat
  { dirfd :: Dirfd,
    pathname :: Path,
    flags :: Flags,
    mode :: Maybe Flags,
    ret :: Either Errno FileDescriptor
  }
  deriving (Show)

data Close = MkClose
  { fd :: FileDescriptor
  , ret :: Maybe Errno
  }
  deriving (Show)

data Execve = MkExecve
  { pathname :: Path,
    argv :: [Text],
    envp :: [Text],
    ret :: Maybe Errno
  }
  deriving (Show)

data Read_ = MkRead
  { fd :: FileDescriptor,
    buf :: Text, -- TODO: ByteString?
    count :: ByteCount,
    ret :: Either Errno ByteCount
  }
  deriving (Show)

-- | The name of a system call.
newtype SystemCallName = SystemCallName Text
  deriving (Eq, Ord, Show, IsString)

-- | The name of a signal.
newtype SignalName = SignalName Text
  deriving (Eq, Ord, Show, IsString)

-- | Indicates whether a system call was interrupted.
data SystemCallStatus = Finished | Unfinished | Resumed
  deriving (Eq, Ord, Show)

-- | See <https://man7.org/linux/man-pages/man3/errno.3.html>.
newtype Errno = Errno Text
  deriving (Eq, Ord, Show, IsString)

-- | A file system path name.
type Path = Text

-- | A file descriptor.
data FileDescriptor = FileDescriptor Fd Path
  deriving (Show)

-- | A directory file descriptor.
-- See <https://man7.org/linux/man-pages/man2/openat.2.html>.
data Dirfd = AT_FDCWD | Dirfd FileDescriptor
  deriving (Show)

type Mode = Flags

type StatStruct = Text -- TODO

-- | See <https://man7.org/linux/man-pages/man2/sigaction.2.html>.
type SigInfo = Text -- TODO

-- | A set of symbolicated flag arguments.
type Flags = Set Text
