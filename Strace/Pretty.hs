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
prettyLine (Line (Just pid) (Just t) ev) = printf "%s  %s %s" (prettyPID pid) (prettyTime t) (prettyEvent ev)

prettyTime :: Time -> String
prettyTime (Instant t) = printf "%d.%d" (systemSeconds t) (systemNanoseconds t `div` 1000)

prettyEvent :: Event -> String
prettyEvent = \case
  SystemCall (OtherSystemCall name args Finished) -> printf "%s%s" (prettySysCallName name) args
  SystemCall (OtherSystemCall name args Unfinished) -> printf "%s%s <unfinished ...>" (prettySysCallName name) args
  SystemCall (OtherSystemCall name args Resumed) -> printf "<... %s resumed>%s" (prettySysCallName name) args
  Exit n -> printf "+++ exited with %d +++" n
  Signal (OtherSignal name info) -> printf "--- %s %s ---" (prettySignalName name) info
  _ -> undefined


prettyPID :: PID -> String
prettyPID (PID n) = show n

prettySysCallName :: SystemCallName -> String
prettySysCallName (SystemCallName n) = Text.unpack n

prettySignalName :: SignalName -> String
prettySignalName (SignalName n) = Text.unpack n