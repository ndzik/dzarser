{-# LANGUAGE LambdaCase #-}

module Dzarser.Combinator where

import Control.Applicative as A
  ( Alternative (..),
    Applicative (..),
    optional,
  )
import Data.Char
import Dzarser.Parser
import Text.Printf
import Text.Read (readMaybe)

-- -- With this foundation set, we can start defining some useful combinators.

-- item parses a single char from the stream.
item :: Monad m => ParserT m Char
item = ParserT $ \case
  [] -> return []
  (c : rs) -> return [ParserResult (c, rs)]

peek :: Monad m => ParserT m (Maybe Char)
peek = ParserT $ \case
  [] -> return [ParserResult (Nothing, [])]
  (c : rs) -> return [ParserResult (Just c, c : rs)]

parserFail :: Monad m => String -> ParserT m a
parserFail reason = ParserT $ \s -> return [ParserError $ printf "%s at: '%s'" reason s]

-- number parses a consecutive amount of digits and returns them as an
-- `Integer`
--  >>> Implementing the `Alternative` typeclass for our `ParserT` allows us to
--  use `many`, `some` and `<|>`, which is perfect for this use case.
number :: Monad m => ParserT m Integer
number = do
  some (satisfy isDigit $ \r -> printf "expected '<digit>' got '%c'" r)
    >>= \s ->
      ( \case
          Nothing -> parserFail $ printf "expected digits got: '%c'" s
          Just a -> pure a
      )
        (readMaybe s :: Maybe Integer)

space :: Monad m => ParserT m Char
space = satisfy isSpace $ \r -> printf "expected '%s' got '%c'" "<space>" r

-- spaces skips all upcoming spaces.
spaces :: Monad m => ParserT m String
spaces = many space

-- satisfy uses the input predicate and returns the expected token when
-- encountered.
satisfy :: Monad m => (Char -> Bool) -> (Char -> String) -> ParserT m Char
satisfy p mkErr =
  peek >>= \case
    Nothing -> parserFail "EOF"
    Just c -> if p c then item else parserFail $ mkErr c

expect :: Monad m => Char -> ParserT m Char
expect c = satisfy (c ==) $ \r -> printf "expected '%c' got '%c'" c r

optional :: Monad m => ParserT m a -> ParserT m (Maybe a)
optional = A.optional

--optional :: Parser a -> Parser a
--optional p = Parser $ \s -> case parse p s of
--  [ParserError err] -> [ParserNoop s]
--  v                 -> v

name :: Monad m => ParserT m String
name = some (satisfy pred $ \r -> printf "expected letters but got: '%c'" r)
  where
    pred c = isLetter c || c == '_'
