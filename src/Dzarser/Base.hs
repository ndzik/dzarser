{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Dzarser.Base where

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
import Text.Printf

-- ParserResult describes the result of a `Parser`, which can either be a
-- successful parse, or a a `ParseError`.
data ParserResult a = ParserResult (a, String) | ParserError String Int Int deriving (Eq)

instance Show a => Show (ParserResult a) where
  show (ParserResult (a, s)) = "parsed: " <> show a <> " remaining stream: " <> s
  show (ParserError msg c l) = printf "error at [Col %i | Line %i]: %s" c l msg

type ParseError = String

-- A `Parser` is a function from `String`s to things `a` and `String`s.
-- Parameterized by `s` the parsers state and `m` the internal monad.
newtype ParserT s m a = ParserT {parse :: String -> StateT s m [ParserResult a]}

-- runParserT runs the given parser on the given input and returns the result.
runParserT :: (Show a, Monad m) => ParserT s m a -> s -> String -> m (Either ParseError a, s)
runParserT p defState s =
  runStateT (parse p s) defState >>= \case
    ([ParserResult (a, _)], s) -> return (Right a, s)
    ([err@ParserError {}], s) -> return (Left $ show err, s)
    (v, s) -> return (Left $ "parse error: unknown with remainder: " ++ show v, s)

debugParserT :: (Show a, Monad m) => ParserT s m a -> s -> String -> m [ParserResult a]
debugParserT p defState s = evalStateT (parse p s) defState

class ParserTracker s where
  incCol :: s -> s
  incLine :: s -> s
  curPos :: s -> (Int, Int)

instance MonadTrans (ParserT s) where
  lift eff = ParserT $ \s -> do
    a <- lift eff
    return [ParserResult (a, s)]

instance Monad m => MonadState s (ParserT s m) where
  get = ParserT $ \s -> get >>= \st -> return [ParserResult (st, s)]
  put st = ParserT $ \s -> put st >> return [ParserResult ((), s)]

instance Functor ParserResult where
  fmap _ (ParserError e c l) = ParserError e c l
  fmap f (ParserResult r) = ParserResult $ first f r

instance Applicative ParserResult where
  pure a = ParserResult (a, "")
  ParserError e c l <*> _ = ParserError e c l
  _ <*> ParserError e c l = ParserError e c l
  (ParserResult (f, _)) <*> ra = f <$> ra

instance Functor m => Functor (ParserT s m) where
  -- Parse with `Parser` p and map `f` over the results. Remember that a parse
  -- operation might have multiple outcomes, which means we have to map over
  -- every possible outcome.
  fmap f p = ParserT $ fmap (map (f <$>)) <$> parse p

instance Monad m => Applicative (ParserT s m) where
  pure v = ParserT $ \s -> return [ParserResult (v, s)]
  (ParserT p) <*> (ParserT q) =
    ParserT
      ( p
          >=> ( fmap join
                  . mapM
                    ( \case
                        rf@(ParserResult (_, s)) -> fmap (map (rf <*>)) . q $ s
                        ParserError e c l -> return [ParserError e c l]
                    )
              )
      )

--  Alternative using list comprehension and BEFORE `Parser` became a
--  MonadTransformer:
--  p <*> q = Parser $ \s ->
--    [ rf <*> ra | rf@(ParserResult (_, s')) <- parse p s, ra <- parse q s' ]

instance Monad m => Monad (ParserT s m) where
  -- Binding a `Parser` to another `Parser` composes the second parse operation
  -- over the result of the first parse operation, yielding a new `Parser`.
  -- Alternative before `Parser` became a MonadTransformer:
  -- Parser $ \s -> [ res | (a, s') <- p s, res <- parse (f a) s' ]
  (ParserT p) >>= f =
    ParserT $
      p
        >=> fmap join
          . mapM
            ( \case
                ParserResult (a, s) -> parse (f a) s
                ParserError e c l -> return [ParserError e c l]
            )

instance Monad m => Alternative (ParserT s m) where
  empty = ParserT $ do
    return . const [ParserError "no result" 0 0]
  p <|> q = ParserT $ \s ->
    parse p s >>= \case
      [] -> parse q s
      [err@ParserError {}] -> parse q s
      res -> return res

-- Necessary for mtl usage.
instance Monad m => MonadPlus (ParserT s m) where
  mzero = empty
  mplus = (<|>)
