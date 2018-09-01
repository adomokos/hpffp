module Hpffp.ParsingPositiveIntegerSpec where

import Test.Hspec
import Control.Applicative
import Text.Trifecta

main :: IO ()
main = hspec spec

parseDigit :: Parser Char
parseDigit = oneOf "0123456789" <?> "a digit between 0 and 9"

base10Integer :: Parser Integer
base10Integer = read <$> some parseDigit

base10Integer' :: Parser Integer
base10Integer' = do
    sign <- optional (char '-')
    xs <- some parseDigit
    case sign of
      Nothing -> return (read xs)
      Just x -> return (read $ x:xs)

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

spec :: Spec
spec = do
    describe "Parsing Exercises" $ do
        it "parses numbers" $ do
            let result = maybeSuccess (parseString parseDigit mempty "123")
            result `shouldBe` Just '1'
            let result = maybeSuccess (parseString parseDigit mempty "abc")
            result `shouldBe` Nothing
            let result = maybeSuccess (parseString base10Integer mempty "123abc")
            result `shouldBe` Just 123
            let result = maybeSuccess (parseString base10Integer mempty "abc")
            result `shouldBe` Nothing
        it "parses negative numbers as well" $ do
            let result = maybeSuccess (parseString base10Integer' mempty "-123abc")
            result `shouldBe` Just (-123)
