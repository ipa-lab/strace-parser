{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module Strace.Finish (finishSystemCalls) where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Prelude hiding (lookup)
import Strace.Types
import Streaming
import Streaming.Internal

type SysCallMap = IntMap SystemCall

insert pid = IntMap.insert (fromEnum pid)
lookup pid = IntMap.lookup (fromEnum pid)
delete pid = IntMap.delete (fromEnum pid)

finishSystemCalls :: Monad m => Stream (Of Line) m r -> Stream (Of Line) m r
finishSystemCalls = metamorph produce consume (mempty, Nothing)
 where
  consume (!m, _) l@(Line pid _ e) = case e of
    SystemCall c@(Unknown _ _ Unfinished) -> (insert pid c m, Nothing)
    _                                     -> (             m, Just l )

  produce (!m, k) = case k of
    Just (Line pid t2 e)
      | SystemCall (Unknown _              args2  Resumed   ) <- e
      , Just       (Unknown name  args1           Unfinished) <- lookup pid m
      , let c' =   (Unknown name (args1 <> args2) Finished  )
      , let l' = Line pid t2 (SystemCall c')
            -> Just (l', (delete pid m, Nothing))
    Just l  -> Just (l , (           m, Nothing))
    Nothing -> Nothing

------------------------------------------------------------------------------

metamorph :: Monad m 
          => (s -> Maybe (b, s))  -- produces output stream elements from state
          -> (s -> a -> s)        -- consumes input stream elements into state
          -> s                    -- initial state
          -> Stream (Of a) m r    -- input stream
          -> Stream (Of b) m r    -- output stream
metamorph f g = loop
  where
    loop !s stream = case f s of
      Just (b, s') -> Step (b :> loop s' stream)
      Nothing -> case stream of
        Return r -> Return r
        Effect m -> Effect (liftM (loop s) m)
        Step (a :> rest) -> loop (g s a) rest
