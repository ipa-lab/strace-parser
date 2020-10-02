{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Strace.Pretty where

import Strace.Types
import Text.Printf
import Data.Time.Clock.System
import Data.List
import qualified Data.Text as Text

prettyTrace :: Trace -> String
prettyTrace = intercalate "\n" . map prettyLine

prettyLine :: Line Event -> String
prettyLine (Line pid t ev) = printf "%s  %s %s" (show pid) (prettyTime t) (prettyEvent ev)

prettyTime :: SystemTime -> String
prettyTime t = printf "%d.%d" (systemSeconds t) (systemNanoseconds t `div` 1000)

prettyEvent :: Event -> String
prettyEvent = \case
  SystemCall syscall -> prettySysCall syscall
  Exit n -> printf "+++ exited with %d +++" n
  Signal (OtherSignal name info) -> printf "--- %s %s ---" (prettySignalName name) info
  Killed name -> printf "+++ killey by %s +++" (prettySignalName name)

prettySysCall :: SystemCall -> String
prettySysCall = \case
  OtherSystemCall name args Finished -> printf "%s%s" (prettySysCallName name) args
  OtherSystemCall name args Unfinished -> printf "%s%s <unfinished ...>" (prettySysCallName name) args
  OtherSystemCall name args Resumed -> printf "<... %s resumed>%s" (prettySysCallName name) args
  Execve path args env errno -> printf "execve %s %s <%d env vars> %s" (show path) (show args) (length env) (show errno)
  Openat dirfd path flags fd -> printf "openat %s %s %s = %s" (show dirfd) (show path) (show flags) (show fd)
  Close fd retval -> printf "close %s = %s" (prettyFd fd) (show retval)
  Read fd buf count retval -> printf "read %s <%d bytes> %s = %s" (prettyFd fd) (Text.length buf) (show count) (show retval)
  Stat path (Left errno) -> printf "stat %s = -1 %s" (show path) (show errno)
  Stat path (Right _) -> printf "stat %s <struct> = 0" (show path)
  _ -> undefined

prettySysCallName :: SystemCallName -> String
prettySysCallName (SystemCallName n) = Text.unpack n

prettySignalName :: SignalName -> String
prettySignalName (SignalName n) = Text.unpack n

prettyFd :: FileDescriptor -> String
prettyFd (FileDescriptor fd path) = printf "%s<%s>" (show fd) (Text.unpack path)