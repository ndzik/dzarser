module Dzarser.Test.StatefulParserSpec where

import Control.Applicative ((<|>))
import Dzarser.Base
import Dzarser.Combinator
import Dzarser.Stateful.Parser
import Test.Hspec
  ( Spec,
    describe,
    it,
  )
import Test.Hspec.Expectations.Pretty

spec :: Spec
spec = do
  describe "StatefulParser" $ do
    it "tracks position" $ do
      snd (runParser (optional space *> number *> optional space *> number) "1234\n42069") `shouldBe` ParserState 2 10
    it "backtracks position for optionals" $ do
      snd (runParser (optional space *> (name' <|> number) *> optional space *> number) "1234\n42069") `shouldBe` ParserState 2 10

name' :: (Monad m, ParserTracker s) => ParserT s m Integer
name' = item >> item >> name >> parserFail "lol"
