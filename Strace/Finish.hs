-- | Pass to stitch together unfinished system calls.
module Strace.Finish (finishSystemCalls) where

import Data.Coerce
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Strace.Types
import Debug.Trace
import Data.Foldable

finishSystemCalls :: Trace -> Trace
finishSystemCalls = reverse . snd . foldl' go (emptyState, [])
  where
    go :: (State, Trace) -> Line Event -> (State, Trace)
    go (s, ls) l@(Line (Just pid) time e) = traceShow l $ case e of
      SystemCall (OtherSystemCall _ _ Unfinished) -> case suspend l s of
        Left err -> error err -- TODO
        Right s' -> (s', ls)
      SystemCall (OtherSystemCall _ _ Resumed) -> case resume l s of
        Left err -> error err -- TODO
        Right (s', l') -> (s', l' : ls)
      _ -> (s, l : ls)

newtype State = State (IntMap (Line Event))
  deriving (Show)

emptyState :: State
emptyState = State IntMap.empty

suspend :: Line Event -> State -> Either String State
suspend (Line Nothing _ _) _ = Left "unfinished syscall without PID"
suspend l@(Line (Just pid) _ e) (State s) = case e of
  SystemCall (OtherSystemCall name _ Unfinished) -> case IntMap.lookup (fromEnum pid) s of
    Just _ -> Left $ "inconsistent trace at " ++ show l -- TODO: better msg
    Nothing -> Right $ State $ IntMap.insert (fromEnum pid) l s
  _ -> Left "impossible"

resume :: Line Event -> State -> Either String (State, Line Event)
resume (Line Nothing _ _) _ = Left "unfinished syscall without PID"
resume l@(Line (Just pid) t2 e) (State s) = case e of
  SystemCall (OtherSystemCall name2 args2 Resumed) -> case IntMap.lookup (fromEnum pid) s of
    Nothing -> Left $ "inconsistent trace at " ++ show (l,s) -- TODO: better msg
    Just (Line _ t1 (SystemCall (OtherSystemCall name1 args1 Unfinished)))
      | name1 == name2 ->
        let s' = State $ IntMap.delete (fromEnum pid) s
            l' = Line (Just pid) t2 e' -- TODO: fix time
            e' = SystemCall (OtherSystemCall name1 (args1 <> args2) Finished)
         in Right (s', l')
    _ -> Left $ "inconsistent trace at " ++ show l -- TODO: better msg
  _ -> Left "impossible"