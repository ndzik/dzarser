{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Dzarser.Parser where

import Control.Applicative as A
  ( Alternative (..),
    Applicative (..),
    optional,
  )
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans
import Data.Bifunctor (first)
import Data.Functor.Identity
import Data.Traversable (sequence)
import Dzarser.Base

type Parser a = ParserT (Identity ()) Identity a

instance ParserTracker (Identity ()) where
  incCol _ = return ()
  incLine _ = return ()
  curPos _ = (0, 0)

runParser :: Show a => Parser a -> String -> Either ParseError a
runParser p = fst . runIdentity . runParserT p (Identity ())

debugParser :: Show a => Parser a -> String -> [ParserResult a]
debugParser p s = runIdentity . evalStateT (parse p s) $ Identity ()
