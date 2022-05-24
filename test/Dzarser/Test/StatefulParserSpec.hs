module Dzarser.Test.StatefulParserSpec where

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
