-- | Pass to resolve file descriptors.
module Strace.FileDescriptors (resolveFileDescriptors) where

import Strace.Types

resolveFileDescriptors :: Trace -> Trace
resolveFileDescriptors = undefined -- TODO: fold with state