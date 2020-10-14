{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

-- | Pass to stitch together unfinished system calls.
module Strace.Finish (finishSystemCalls) where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Strace.Types
import Streaming
import Streaming.Internal

data Pair a b = Pair !a !b

type SysCallMap = IntMap Line

finishSystemCalls :: Monad m => Stream (Of Line) m r -> Stream (Of Line) m r
finishSystemCalls = metamorph produce consume (Pair mempty [])

consume :: Pair SysCallMap [Line] -> Line -> Pair SysCallMap [Line]
consume (Pair m ls) l = case l of
  Line pid _ (SystemCall (OtherSystemCall _ _ Unfinished)) ->
    let m' = IntMap.insert (fromEnum pid) l m in Pair m' []
  _ -> Pair m (l : ls)

produce :: Pair SysCallMap [Line] -> Maybe (Line, Pair SysCallMap [Line])
produce (Pair _ []) = Nothing
produce (Pair m (l : ls)) = case l of
  Line pid t2 (SystemCall (OtherSystemCall name2 args2 Resumed))
    | Just (Line _ _ (SystemCall c1)) <- IntMap.lookup (fromEnum pid) m,
      OtherSystemCall name1 args1 Unfinished <- c1,
      name1 == name2 ->
      let c' = OtherSystemCall name1 (args1 <> args2) Finished
          l' = Line pid t2 (SystemCall c') -- TODO: merge t1 and t2 ?
          m' = IntMap.delete (fromEnum pid) m
       in Just (l', Pair m' ls)
  _ -> Just (l, Pair m ls)

metamorph :: Monad m => (s -> Maybe (b, s)) -> (s -> a -> s) -> s -> Stream (Of a) m r -> Stream (Of b) m r
metamorph f g = loop
  where
    loop !s stream = case f s of
      Just (b, s') -> Step (b :> loop s' stream)
      Nothing -> case stream of
        Return r -> Return r
        Effect m -> Effect (liftM (loop s) m)
        Step (a :> rest) -> loop (g s a) rest
