module Dzarser.Test.ParserSpec where

import Control.Exception
import Control.Monad
import Dzarser.Parser
import Dzarser.Combinator
import Test.Hspec
  ( Spec,
    describe,
    it,
  )
import Test.Hspec.Expectations.Pretty

data TestNumber = TestNumber Integer String Integer
  deriving (Eq, Show)

mkTestNumber :: Parser (Integer -> String -> Integer -> TestNumber)
mkTestNumber = pure TestNumber

spec :: Spec
spec = do
  describe "Parser" $ do
    it "parses numbers" $ do
      runParser number "1234" `shouldBe` Right 1234
    it "allows optional chaining" $ do
      runParser (optional space *> number) "1234" `shouldBe` Right 1234
      runParser (optional space *> number) " 1234" `shouldBe` Right 1234
      runParser (optional space *> number) "  1234"
        `shouldBe` Left "parse error: expected '<digit>' got ' ' at: ' 1234'"
    it "parses strings" $ do
      runParser name "nice_name" `shouldBe` Right "nice_name"
      runParser name "+" `shouldBe` Left "parse error: expected letters but got: '+' at: '+'"
    it "parses alternating sequences" $ do
      runParser (number *> name *> number) "123asdf123" `shouldBe` Right 123
    it "correctly implements the applicative instance" $ do
      runParser (TestNumber <$> number <*> name <*> number) "123asdf123"
        `shouldBe` Right (TestNumber 123 "asdf" 123)
