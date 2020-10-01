{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Bifunctor
import Data.Char
import Data.Coerce
import Data.List
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Time.Clock.System
import Data.Void
import Data.Word
import Strace.Finish
import Strace.Raw
import Strace.Types
import System.Environment
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Text.ParserCombinators.ReadP (ReadP, readP_to_S)
import Text.Printf
import Text.Read.Lex (readHexP, readOctP)
import Strace.Pretty
import Strace.Events

-- We assume the following strace invocation: 
--
--      strace -f -ttt -v -x -s 1048576
--
-- This follows forks and shows PIDs (-f), prints timestamps in the right format
-- (-ttt), expands all structures incl. env vars (-v), prints non-ascii strings
-- in hex format (-x) and prints strings up to 1 MB (-s 1048576).
--

-- TODO: check whether -x is necessary

main :: IO ()
main = do
  [straceFile] <- getArgs

  r1 <- parseRawTrace straceFile
  case r1 of
    Left err -> putStrLn err
    Right t1_ -> do
      let t1 = t1_ --take 30 t1_

      print (length t1)
      -- putStrLn $ prettyTrace t1

      let t2 = finishSystemCalls t1
      print (length t2)
      --putStrLn $ prettyTrace t2

      let isFoo (Line _ _ (SystemCall (OtherSystemCall _ _ _))) = False
          isFoo (Line _ _ (SystemCall _)) = True
          isFoo _ = False

      let t3 = filter isFoo $ parseEvents t2
      print (length t3)
      putStrLn $ prettyTrace t3

  -- r <- do
  --   t1 <- liftIO $ parseRawTrace straceFile
  --   let t2 = finishSystemCalls t1
  --   pure t2

  -- --     let r = do
  -- --             t1 <- f1 straceFile --Strace.Raw.parseRawTrace straceFile
  -- --             --t2 <- f2 t1 --Strace.Finish.finishSystemCalls t1
  -- --             --t3 <- f3 t2 --Strace.Events.parseEvents t2
  -- --             --t4 <- f4 t3 --Strace.FileDescriptors.resolveFileDescriptors t3
  -- --             --return t4

  -- case r of
  --   Left err -> putStrLn err
  --   Right trace -> print (length trace) >> print trace

f1 :: Num b => p -> Either String b
f1 x = Right 1

f2 x = Left "hey" --Right (x+1)

f3 x = Right (x + 1)

f4 x = Right (x + 1)

--     r <- Strace.Raw.parse straceFile
--     case r of
--         Left err -> printf err
--         Right trace1 -> do

--     printf "Reading %s ... " straceFile
--     !raw <- Text.readFile straceFile
--     printf "%d characters\n" (Text.length raw)

--     printf "1st pass ... "
--     let r1 =  parse (rawTraceLine `manyTill` eof) straceFile raw

--     case r1 of
--         Left bundle -> printf "error\n" >> putStr (errorBundlePretty bundle)
--         Right lines -> do
--             printf "%d lines\n" (length lines)

--             printf "2nd pass ..."

--             -- forM_ lines $ \(TraceLine _ _ ev) -> do
--             --     case ev of
--             --         RawSystemCall name _ -> Text.putStrLn $ coerce name
--             --         _ -> return ()

--             -- TODO: 2nd pass, where we combine unfinished system calls
--             --printf "2nd pass ..."
--             -- fold with a state set of unfinished (pid,systemcallname)

--             -- TODO: 3rd pass, where we turn RawEvents into Events

--             -- TODO: 4th pass, where we resolve file descriptors (maybe we can do this during 3rd pass)

-- -- line = Line <$> pid <*> timestamp <*> event

-- -- event = syscall <|> syscall_unfinished <|> syscall_resumed <|> signal

-- -- syscall = SysCall <$> name <*> args <* "=" *> retval
