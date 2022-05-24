{-# LANGUAGE LambdaCase #-}

module Dzarser.Parser where

import Control.Applicative as A
  ( Alternative (..),
    Applicative (..),
    optional,
  )
import Control.Monad
import Control.Monad.Trans
import Data.Bifunctor (first)
import Data.Functor.Identity
import Data.Traversable (sequence)

-- ParserResult describes the result of a `Parser`, which can either be a
-- successful parse, or a a `ParseError`.
data ParserResult a = ParserResult (a, String) | ParserError String deriving (Show, Eq)

type ParseError = String

-- A `Parser` is a function from `String`s to things `a` and `String`s.
newtype ParserT m a = ParserT {parse :: String -> m [ParserResult a]}

type Parser a = ParserT Identity a

instance MonadTrans ParserT where
  lift eff = ParserT $ \s -> do
    a <- eff
    return [ParserResult (a, "")]

instance Functor ParserResult where
  fmap _ (ParserError e) = ParserError e
  fmap f (ParserResult r) = ParserResult $ first f r

instance Applicative ParserResult where
  pure a = ParserResult (a, "")
  (ParserError e) <*> _ = ParserError e
  _ <*> (ParserError e) = ParserError e
  (ParserResult (f, _)) <*> ra = f <$> ra

-- runParserT runs the given parser on the given input and returns the result.
runParserT :: (Show a, Monad m) => ParserT m a -> String -> m (Either ParseError a)
runParserT p s =
  parse p s >>= \case
    [ParserResult (a, _)] -> return . Right $ a
    [ParserError e] -> return . Left $ "parse error: " ++ e
    v -> return . Left $ "parse error: unknown with remainder: " ++ show v

debugParserT :: (Show a, Monad m) => ParserT m a -> String -> m [ParserResult a]
debugParserT = parse

runParser :: Show a => Parser a -> String -> Either ParseError a
runParser p = runIdentity . runParserT p

debugParser :: Show a => Parser a -> String -> [ParserResult a]
debugParser p = runIdentity . parse p

instance (Functor m) => Functor (ParserT m) where
  -- Parse with `Parser` p and map `f` over the results. Remember that a parse
  -- operation might have multiple outcomes, which means we have to map over
  -- every possible outcome.
  fmap f p = ParserT $ fmap (map (f <$>)) <$> parse p

instance (Monad m) => Applicative (ParserT m) where
  pure v = ParserT $ \s -> return [ParserResult (v, s)]
  (ParserT p) <*> (ParserT q) =
    ParserT
      ( p
          >=> ( fmap join
                  . mapM
                    ( \case
                        rf@(ParserResult (_, s)) -> fmap (map (rf <*>)) . q $ s
                        ParserError err -> return [ParserError err]
                    )
              )
      )

--  Alternative using list comprehension and BEFORE `Parser` became a
--  MonadTransformer:
--  p <*> q = Parser $ \s ->
--    [ rf <*> ra | rf@(ParserResult (_, s')) <- parse p s, ra <- parse q s' ]

instance (Monad m) => Monad (ParserT m) where
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
                ParserError err -> return [ParserError err]
            )

instance Monad m => Alternative (ParserT m) where
  empty = ParserT $ return . const [ParserError "no result"]
  p <|> q = ParserT $ \s ->
    parse p s >>= \case
      [] -> parse q s
      [ParserError _] -> parse q s
      res -> return res

-- Necessary for mtl usage.
instance Monad m => MonadPlus (ParserT m) where
  mzero = empty
  mplus = (<|>)
