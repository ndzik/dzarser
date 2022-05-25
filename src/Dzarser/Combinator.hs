{-# LANGUAGE LambdaCase #-}

module Dzarser.Combinator where

import Control.Applicative as A
  ( Alternative (..),
    Applicative (..),
    optional,
  )
import Control.Monad.State
import Data.Char
import Dzarser.Base
import Text.Printf
import Text.Read (readMaybe)

-- -- With this foundation set, we can start defining some useful combinators.

item :: (Monad m, ParserTracker s) => ParserT s m Char
item = ParserT $ \case
  [] -> return []
  (c : rs) -> trackPos c >> return [ParserResult (c, rs)]
  where
    trackPos :: (Monad m, ParserTracker s) => Char -> StateT s m ()
    trackPos '\n' = modify incLine
    trackPos c = modify incCol

peek :: Monad m => ParserT s m (Maybe Char)
peek = ParserT $ \case
  [] -> return [ParserResult (Nothing, [])]
  (c : rs) -> return [ParserResult (Just c, c : rs)]

parserFail :: (Monad m, ParserTracker s) => String -> ParserT s m a
parserFail reason = ParserT $ \s -> do
  (c, l) <- gets curPos
  return [ParserError (printf "%s at: '%s'" reason s) c l]

-- number parses a consecutive amount of digits and returns them as an
-- `Integer`
--  >>> Implementing the `Alternative` typeclass for our `ParserT` allows us to
--  use `many`, `some` and `<|>`, which is perfect for this use case.
number :: (Monad m, ParserTracker s) => ParserT s m Integer
number = do
  some (satisfy isDigit $ \r -> printf "expected '<digit>' got '%c'" r)
    >>= \s ->
      ( \case
          Nothing -> parserFail $ printf "expected digits got: '%c'" s
          Just a -> pure a
      )
        (readMaybe s :: Maybe Integer)

space :: (Monad m, ParserTracker s) => ParserT s m Char
space = satisfy isSpace $ \r -> printf "expected '%s' got '%c'" "<space>" r

-- spaces skips all upcoming spaces.
spaces :: (Monad m, ParserTracker s) => ParserT s m String
spaces = many space

-- satisfy uses the input predicate and returns the expected token when
-- encountered.
satisfy :: (Monad m, ParserTracker s) => (Char -> Bool) -> (Char -> String) -> ParserT s m Char
satisfy p mkErr =
  peek >>= \case
    Nothing -> parserFail "EOF"
    Just c -> if p c then item else parserFail $ mkErr c

expect :: (Monad m, ParserTracker s) => Char -> ParserT s m Char
expect c = satisfy (c ==) $ \r -> printf "expected '%c' got '%c'" c r

optional :: Monad m => ParserT s m a -> ParserT s m (Maybe a)
optional = A.optional

name :: (Monad m, ParserTracker s) => ParserT s m String
name = some (satisfy pred $ \r -> printf "expected letters but got: '%c'" r)
  where
    pred c = isLetter c || c == '_'
