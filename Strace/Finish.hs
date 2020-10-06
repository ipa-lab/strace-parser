{-# LANGUAGE LambdaCase #-}

-- | Pass to stitch together unfinished system calls.
module Strace.Finish (finishSystemCalls) where

import Data.Coerce
import Data.Foldable
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Debug.Trace
import Strace.Types

type SysCallMap = IntMap Line

finishSystemCalls :: Trace -> Trace
finishSystemCalls = reverse . snd . foldl' go (mempty, [])
  where    
    go :: (SysCallMap, Trace) -> Line -> (SysCallMap, Trace)
    go (m, ls) l@(Line pid t2 (SystemCall c2))
      | OtherSystemCall _ _ Unfinished <- c2 =
        let m' = IntMap.insert (fromEnum pid) l m in (m', ls)
      | OtherSystemCall name2 args2 Resumed <- c2,
        Just (Line _ t1 (SystemCall c1)) <- IntMap.lookup (fromEnum pid) m,
        OtherSystemCall name1 args1 Unfinished <- c1,
        name1 == name2 =
        let m' = IntMap.delete (fromEnum pid) m
            l' = Line pid t1 (SystemCall c') -- TODO: merge t1 and t2 ?
            c' = OtherSystemCall name1 (args1 <> args2) Finished
         in (m', l' : ls)
    go (m, ls) l = (m, l : ls)
