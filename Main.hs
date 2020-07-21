{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import System.Environment
import Text.Printf
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Void
import Data.Word
import Data.List
import Control.Monad
import Data.Maybe

-- TODO/NOTES:
-- with the -v option, environment vars (might) get expanded
-- (but might need to increase string length to actually see)

main :: IO ()
main = do
    [straceFile,numLines] <- getArgs
    printf "Parsing %s ...\n" straceFile

    let strace :: Parser Strace
        strace = count (read numLines) (traceLine <* eol)
    
    result <- parse strace straceFile <$> Text.readFile straceFile
    case result of
        Left bundle -> putStr (errorBundlePretty bundle)
        Right x -> putStrLn $ concat $ intersperse "\n" $ map prettyLine x

type Parser = Parsec Void Text

-- strace :: Parser Strace
-- strace = count 3 (traceLine <* eol)

type Strace = [TraceLine]

data TraceLine = TraceLine
    { pid :: Word32  -- TODO: check that this always fits
    , timestamp :: Float -- TODO: clearly wrong; maybe SystemTime?
    , syscall :: Text
    , args :: [Text]
    , retval :: Text -- TODO: split into retval and retinfo
    }
    deriving (Show)

prettyLine :: TraceLine -> String
prettyLine TraceLine{..} = 
    printf "%d %f %s%s = %s" pid timestamp syscall (parens (prettyArgs args)) retval
  where
      prettyArgs = concat . intersperse comma . map bg . map Text.unpack
      comma = ", " --"\x001b[31;1m,\x001b[0m "
      parens :: String -> String
      parens = printf "(%s)" --printf "\x001b[31;1m(\x001b[0m%s\x001b[31;1m)\x001b[0m"

      bg :: String -> String
      bg = printf "\x001b[43m%s\x001b[0m"

traceLine :: Parser TraceLine
traceLine = do
    pid <- lexeme L.decimal
    timestamp <- lexeme L.float
    syscall <- takeWhileP (Just "syscall") (/= '(')
    args <- between (char '(') (char ')') (syscallArg `sepBy` (lexeme (char ',')))
    space >> char '=' >> space
    retval <- takeWhileP (Just "retval") (/= '\n')
    return TraceLine{..}

lexeme :: Parser a -> Parser a
lexeme  = L.lexeme (L.space space1 empty empty)

data SyscallArg = String Text | Struct Text | Pointer Word64 | Other Text
    deriving (Show)

syscallArg :: Parser Text
syscallArg = choice 
    [ stringLiteral
    , structLiteral
    , arrayLiteral
    , Text.pack <$> many (noneOf [',',')'])
    ]

stringLiteral :: Parser Text
stringLiteral = label "string" $ do
    void (char '"')
    str <- manyTill L.charLiteral (char '"')
    trunc <- optional (string "...")
    case trunc of
        Nothing -> return $ Text.pack $ '"' : str ++ "\""
        Just _  -> return $ Text.pack $ '"' : str ++ "\"..."

structLiteral :: Parser Text
structLiteral = label "struct" $ do    
    void (char '{')
    str <- manyTill anySingle (char '}')
    return $ Text.pack $ '{' : str ++ "}"

arrayLiteral :: Parser Text
arrayLiteral = label "array" $ do
    void (char '[')
    str <- manyTill anySingle (char ']')
    return $ Text.pack $ '[' : str ++ "]"
