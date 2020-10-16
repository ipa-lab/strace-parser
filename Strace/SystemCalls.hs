{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Pass to parse system calls and signals and their arguments.
module Strace.SystemCalls (parseEvents, parseSystemCall) where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 qualified as BS
import Data.Int
import Data.Proxy
import Data.Word
import Debug.Trace
import Foreign.C.Types
import GHC.Generics
import Strace.Parser
import Strace.Types
import System.Posix.Types

parseEvents :: Trace -> Trace
parseEvents = map $ mapEvent $ mapSystemCall parseSystemCall

parseSystemCall :: SystemCall -> SystemCall
parseSystemCall c@(OtherSystemCall (SystemCallName name) args Finished) = case name of
  "close" -> parse $ Close <$> parser
  "connect" -> parse $ Connect <$> parser
  "dup" -> parse $ Dup <$> parser
  "dup2" -> parse $ Dup2 <$> parser
  "dup3" -> parse $ Dup3 <$> parser
  "execve" -> parse $ Execve <$> parser
  "fstat" -> parse $ Fstat <$> parser
  "fstatat" -> parse $ Fstatat <$> parser
  "fstatfs" -> parse $ Fstatfs <$> parser
  "lstat" -> parse $ Lstat <$> parser
  "openat" -> parse $ Openat <$> parser
  "pipe" -> parse $ Pipe <$> parser
  "read" -> parse $ Read <$> parser
  "rmdir" -> parse $ Rmdir <$> parser
  "rt_sigaction" -> parse $ Rtsigaction <$> parser
  "stat" -> parse $ Stat <$> parser
  "statfs" -> parse $ Statfs <$> parser
  "write" -> parse $ Write <$> parser
  _ -> c
  where
    --parse f = fromMaybe c $ parseMaybe f args
    parse f = case parseOnly f args of
      Left err -> trace ("error parsing " ++ BS.unpack name ++ ": " ++ err ++ "\n\t" ++ show args) c
      Right x -> x
parseSystemCall x = x

eitherErrnoOr :: Parser a -> Parser (Either Errno a)
eitherErrnoOr p =
  ("-1" *> skipHorizontalSpace *> (Left <$> parseErrno) <* takeByteString) <|> (Right <$> p)

maybeErrno :: Parser (Maybe Errno)
maybeErrno = either Just (const Nothing) <$> eitherErrnoOr "0"

arg0 :: Parser a -> Parser a
arg0 p = "(" *> p

arg :: Parser a -> Parser a
arg p = ", " *> p

retp :: Parser a -> Parser a
retp p = ")" *> skipHorizontalSpace *> "=" *> skipHorizontalSpace *> p

data Pos = S | L | M | R

type family MoveL (p :: Pos) :: Pos where
  MoveL 'S = 'L
  MoveL 'L = 'L
  MoveL _ = 'M

type family MoveR (p :: Pos) :: Pos where
  MoveR 'S = 'R
  MoveR 'R = 'R
  MoveR _ = 'M

class Parsable a where
  parser :: Parser a
  default parser :: (Generic a, GParsable 'S a (Rep a)) => Parser a
  parser = fmap to (gparser @'S @a)
  {-# INLINE parser #-}

class GParsable (p :: Pos) r f where
  gparser :: Parser (f r)

instance (GParsable (MoveL p) r a, GParsable (MoveR p) r b) => GParsable p r (a :*: b) where
  gparser = (:*:) <$> gparser @(MoveL p) <*> gparser @(MoveR p)

instance GParsable p r a => GParsable p r (M1 i c a) where
  gparser = M1 <$> gparser @p

-- no arguments
instance Parsable a => GParsable 'S r (K1 i a) where
  gparser = K1 <$> (arg0 (pure ()) *> retp parser)

-- the first argument
instance Parsable a => GParsable 'L r (K1 i a) where
  gparser = K1 <$> (arg0 parser)

-- some argument
instance Parsable a => GParsable 'M r (K1 i a) where
  gparser = K1 <$> (arg parser)

-- an optional argument
instance {-# OVERLAPPING #-} Parsable a => GParsable 'M r (K1 i (Maybe a)) where
  gparser = K1 <$> (optional $ arg parser)

-- the return value
instance Parsable a => GParsable 'R r (K1 i (Either Errno a)) where
  gparser = K1 <$> (retp $ eitherErrnoOr parser)

-- the return value (0 or errno)
instance GParsable 'R r (K1 i (Maybe Errno)) where
  gparser = K1 <$> (retp maybeErrno)

deriving instance Parsable Close

deriving instance Parsable Connect

deriving instance Parsable Dup

deriving instance Parsable Dup2

deriving instance Parsable Dup3

deriving instance Parsable Execve

deriving instance Parsable Fstat

deriving instance Parsable Fstatat

deriving instance Parsable Lstat

deriving instance Parsable Openat

deriving instance Parsable Pipe

deriving instance Parsable Read_

deriving instance Parsable Rmdir

deriving instance Parsable Rtsigaction

deriving instance Parsable Stat

deriving instance Parsable Statfs

deriving instance Parsable Fstatfs

deriving instance Parsable Write

instance Parsable FileDescriptor where
  parser = fileDescriptor

instance Parsable a => Parsable (Pointer a) where
  parser = pointerTo parser

instance Parsable a => Parsable [a] where
  parser = arrayOf parser

instance Parsable Errno where
  parser = parseErrno

instance Parsable Struct where
  parser = struct

instance Parsable Path where
  parser = path

instance Parsable CSocklen where
  parser = decimal

instance Parsable CSize where
  parser = decimal

instance Parsable Flags where
  parser = parseFlags

instance Parsable Str where
  parser = str

instance Parsable Dirfd where
  parser = pDirfd

instance Parsable SignalName where
  parser = signalName
