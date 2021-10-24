{-# LANGUAGE LambdaCase #-}
module Dzarser.Parser where

import           Control.Applicative           as A
                                                ( Alternative(..)
                                                , Applicative(..)
                                                , optional
                                                )
import           Control.Monad
import           Data.Bifunctor                 ( first )
import           Data.Char
import           Text.Printf
import           Text.Read                      ( readMaybe )

-- ParserResult describes the result of a `Parser`, which can either be a
-- successful parse, or a a `ParseError`.
data ParserResult a = ParserResult (a, String) |  ParserError String deriving (Show, Eq)

-- A `Parser` is a function from `String`s to things `a` and `String`s.
newtype Parser a = Parser { parse :: String -> [ParserResult a] }

instance Functor ParserResult where
  fmap _ (ParserError  e) = ParserError e
  fmap f (ParserResult r) = ParserResult $ first f r

instance Applicative ParserResult where
  pure a = ParserResult (a, "")
  (ParserError e) <*> _               = ParserError e
  _               <*> (ParserError e) = ParserError e
  (ParserResult (f, _)) <*> ra = f <$> ra

-- runParser runs the given parser on the given input and returns the result.
runParser :: Show a => Parser a -> String -> a
runParser p s = case parse p s of
  [ParserResult (a, _)] -> a
  [ParserError  e     ] -> error $ "parse error: " ++ e
  v -> error $ "parse error: unknown with remainder: " ++ show v

debugParser :: Show a => Parser a -> String -> [ParserResult a]
debugParser = parse

instance Functor Parser where
  -- Parse with `Parser` p and map `f` over the results. Remember that a parse
  -- operation might have multiple outcomes, which means we have to map over
  -- every possible outcome.
  fmap f p = Parser $ \s -> map (f <$>) $ parse p s

instance Applicative Parser where
  pure v = Parser $ \s -> [ParserResult (v, s)]
  (Parser p) <*> (Parser q) = Parser
    (p >=> \case
      rf@(ParserResult (_, s)) -> map (rf <*>) $ q s
      ParserError err          -> [ParserError err]
    )
--  p <*> q = Parser $ \s ->
--    [ rf <*> ra | rf@(ParserResult (_, s')) <- parse p s, ra <- parse q s' ]

instance Monad Parser where
  -- Binding a `Parser` to another `Parser` composes the second parse operation
  -- over the result of the first parse operation, yielding a new `Parser`.
  -- Alternative: Parser $ \s -> [ res | (a, s') <- p s, res <- parse (f a) s' ]
  (Parser p) >>= f = Parser $ p >=> \case
    ParserResult (a, s) -> parse (f a) s
    ParserError  err    -> return (ParserError err)


-- With this foundation set, we can start defining some useful `Parser`s.

-- item parses a single char from the stream.
item :: Parser Char
item = Parser $ \case
  []       -> []
  (c : rs) -> [ParserResult (c, rs)]

instance Alternative Parser where
  empty = Parser $ const [ParserError "no result"]
  p <|> q = Parser $ \s -> case parse p s of
    []              -> parse q s
    [ParserError _] -> parse q s
    res             -> res

-- Necessary for mtl usage.
instance MonadPlus Parser where
  mzero = empty
  mplus = (<|>)

parserFail :: String -> Parser a
parserFail reason = Parser $ \s -> [ParserError $ printf "%s at: %s" reason s]

-- number parses a consecutive amount of digits and returns them as an
-- `Integer`
--  >>> Implementing the `Alternative` typeclass for our `Parser` allows us to
--  use `many`, `some` and `<|>`, which is perfect for this use case.
number :: Parser Integer
number = do
  some (satisfy isDigit $ \r -> printf "expected '<digit>' got '%c'" r)
    >>= \s ->
          (\case
              Nothing -> parserFail $ printf "expected digits got: '%c'" s
              Just a  -> pure a
            )
            (readMaybe s :: Maybe Integer)

space :: Parser Char
space = satisfy isSpace $ \r -> printf "expected '%s' got '%c'" "<space>" r

-- spaces skips all upcoming spaces.
spaces :: Parser String
spaces = many space

-- satisfy uses the input predicate and returns the expected token when
-- encountered.
satisfy :: (Char -> Bool) -> (Char -> String) -> Parser Char
satisfy p mkErr = item >>= \c -> if p c then return c else parserFail $ mkErr c

expect :: Char -> Parser Char
expect c = satisfy (c ==) $ \r -> printf "expected '%c' got '%c'" c r

optional :: Parser a -> Parser (Maybe a)
optional = A.optional
--optional :: Parser a -> Parser a
--optional p = Parser $ \s -> case parse p s of
--  [ParserError err] -> [ParserNoop s]
--  v                 -> v

name :: Parser String
name = many (satisfy pred $ \r -> printf "expected letters but got: '%c'" r)
  where pred c = isLetter c || c == '_'
