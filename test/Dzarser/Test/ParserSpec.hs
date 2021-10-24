module Dzarser.Test.ParserSpec where

import           Control.Exception
import           Control.Monad
import           Dzarser.Parser
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                )
import           Test.Hspec.Expectations.Pretty

data TestNumber = TestNumber Integer String Integer
  deriving (Eq, Show)

mkTestNumber :: Parser (Integer -> String -> Integer -> TestNumber)
mkTestNumber = pure TestNumber

spec :: Spec
spec = do
  describe "Parser" $ do
    it "parses numbers" $ do
      runParser number "1234" `shouldBe` 1234
    it "allows optional chaining" $ do
      runParser (optional space *> number) "1234" `shouldBe` 1234
      runParser (optional space *> number) " 1234" `shouldBe` 1234
      evaluate (runParser (optional space *> number) "  1234")
        `shouldThrow` errorCall "parse error: expected '<digit>' got ' ' at: 1234"
    it "parses strings" $ do
      runParser name "nice_name" `shouldBe` "nice_name"
    it "parses alternating sequences" $ do
      runParser (number *> name *> number) "123asdf123" `shouldBe` 123
    it "correctly implements the applicative instance" $ do
      runParser (TestNumber <$> number <*> name <*> number) "123asdf123"
        `shouldBe` TestNumber 123 "asdf" 123
