module Dzarser.Stateful.Parser where

import Control.Monad.State
import Data.Functor.Identity
import Dzarser.Base

data ParserState = ParserState
  { _line :: Int,
    _col :: Int
  }
  deriving (Show, Eq)

defaultParserState :: ParserState
defaultParserState = ParserState 1 1

instance ParserTracker ParserState where
  incCol s = s {_col = _col s + 1}
  incLine s = s {_line = _line s + 1}
  curPos (ParserState l c) = (l, c)

type Parser a = ParserT ParserState Identity a

runParser :: Show a => Parser a -> String -> (Either ParseError a, ParserState)
runParser p = runIdentity . runParserT p defaultParserState

evalParser :: Show a => Parser a -> String -> Either ParseError a
evalParser p = fst . runIdentity . runParserT p defaultParserState

evalParser' :: Show a => Parser a -> ParserState -> String -> Either ParseError a
evalParser' p s = fst . runIdentity . runParserT p s

