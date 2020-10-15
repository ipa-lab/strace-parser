{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

module Strace.Types where

import Data.Set (Set)
import Data.String (IsString)
import Data.Time.Clock.System (SystemTime)
import Data.Word
import System.Posix.Types
import Data.ByteString.Char8 (ByteString)

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
  | Signal SignalName Struct
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
  = OtherSystemCall SystemCallName ByteString SystemCallStatus
  | Close Close
  | Connect Connect
  | Dup Dup
  | Dup2 Dup2
  | Dup3 Dup3
  | Execve Execve
  | Fstat Fstat
  | Fstatat Fstatat
  | Lstat Lstat
  | Openat Openat
  | Pipe Pipe
  | Read Read_
  | Rmdir Rmdir
  | Stat Stat
  | Statfs Statfs
  | Fstatfs Fstatfs
  | Write Write
  deriving (Show)

data Connect = MkConnect
  { sockfd :: FileDescriptor
  , addr :: Pointer Struct
  , addrlen :: CSocklen
  , ret :: Maybe Errno
  }
  deriving (Show)

data Dup = MkDup
  { oldfd :: FileDescriptor,
    ret :: Either Errno FileDescriptor
  }
  deriving (Show)

data Dup2 = MkDup2
  { oldfd :: FileDescriptor,
    newfd :: FileDescriptor,
    ret :: Either Errno FileDescriptor
  }
  deriving (Show)

data Dup3 = MkDup3
  { oldfd :: FileDescriptor,
    newfd :: FileDescriptor,
    flags :: Flags,
    ret :: Either Errno FileDescriptor
  }
  deriving (Show)

data Rmdir = MkRmdir
  { pathname :: Pointer Path,
    ret :: Maybe Errno
  }
  deriving (Show)

data Openat = MkOpenat
  { dirfd :: Dirfd,
    pathname :: Pointer Path,
    flags :: Flags,
    mode :: Maybe Flags,
    ret :: Either Errno FileDescriptor
  }
  deriving (Show)

data Close = MkClose
  { fd :: FileDescriptor,
    ret :: Maybe Errno
  }
  deriving (Show)

data Execve = MkExecve
  { pathname :: Pointer Path,
    argv :: Pointer [Str],
    envp :: Pointer [Str],
    ret :: Maybe Errno
  }
  deriving (Show)

data Read_ = MkRead
  { fd :: FileDescriptor,
    buf :: Pointer Str,
    count :: ByteCount,
    ret :: Either Errno ByteCount
  }
  deriving (Show)

data Write = MkWrite
  { fd :: FileDescriptor,
    buf :: Pointer Str,
    count :: ByteCount,
    ret :: Either Errno ByteCount
  }
  deriving (Show)

data Stat = MkStat
  { pathname :: Pointer Path,
    statbuf :: Pointer Struct,
    ret :: Maybe Errno
  }
  deriving (Show)

data Fstat = MkFstat
  { fd :: FileDescriptor,
    statbuf :: Pointer Struct,
    ret :: Maybe Errno
  }
  deriving (Show)

data Fstatat = MkFstatat
  { dirfd :: Dirfd,
    pathname :: Pointer Path,
    statbuf :: Pointer Struct,
    flags :: Flags,
    ret :: Maybe Errno
  }
  deriving (Show)

data Lstat = MkLstat
  { pathname :: Pointer Path,
    statbuf :: Pointer Struct,
    ret :: Maybe Errno
  }
  deriving (Show)

data Statfs = MkStatfs
  { pathname :: Pointer Path,
    buf :: Pointer Struct,
    ret :: Maybe Errno
  }
  deriving (Show)

data Fstatfs = MkFstatfs
  { fd :: FileDescriptor,
    buf :: Pointer Struct,
    ret :: Maybe Errno
  }
  deriving (Show)

data Pipe = MkPipe
  { pipefd :: Pointer [FileDescriptor],
    ret :: Maybe Errno
  }
  deriving (Show)

-- | A pointer to a potentially dereferenced/symbolicated value.
data Pointer a = Pointer Word64 | Deref a
  deriving (Show)

-- | A possibly truncated ASCII string.
data Str = Complete ByteString | Truncated ByteString
  deriving (Show)

-- | The name of a system call.
newtype SystemCallName = SystemCallName ByteString
  deriving (Eq, Ord, Show, IsString)

-- | The name of a signal.
newtype SignalName = SignalName ByteString
  deriving (Eq, Ord, Show, IsString)

-- | Indicates whether a system call was interrupted.
data SystemCallStatus = Finished | Unfinished | Resumed
  deriving (Eq, Ord, Show)

-- | See <https://man7.org/linux/man-pages/man3/errno.3.html>.
newtype Errno = Errno ByteString
  deriving (Eq, Ord, Show, IsString)

-- | A file system path.
newtype Path = Path ByteString
  deriving (Show)

-- | A file descriptor.
data FileDescriptor = FileDescriptor Fd (Maybe Path)
  deriving (Show)

-- | A directory file descriptor.
-- See <https://man7.org/linux/man-pages/man2/openat.2.html>.
data Dirfd = AT_FDCWD | Dirfd FileDescriptor
  deriving (Show)

-- | A set of symbolicated flag arguments.
type Flags = Set ByteString

-- | A symbolicated string representation of struct data.
type Struct = ByteString
