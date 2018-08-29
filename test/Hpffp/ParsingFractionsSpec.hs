{-# LANGUAGE OverloadedStrings #-}
module Hpffp.ParsingFractionsSpec where

import Test.Hspec
import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta

main :: IO ()
main = hspec spec

badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
    numerator <- decimal
    char '/'
    denominator <- decimal
    return (numerator % denominator)

virtuousFraction :: Parser Rational
virtuousFraction = do
    numerator <- decimal
    char '/'
    denominator <- decimal
    case denominator of
      0 -> fail "Denominator cannot be zero"
      _ -> return (numerator % denominator)

instance (Eq a) => Eq (Result a) where
    (==) (Success x) (Success y) = x == y
    (==) _ _ = False

type NumberOrString =
    Either Integer String

a = "blah"
b = "123"
c = "123blah789"

parseNos :: Parser NumberOrString
parseNos = (Left <$> integer) <|> (Right <$> some letter)

spec :: Spec
spec = do
    describe "Parsing" $ do
        it "pulls parsed chars" $ do
            let parseFraction' =
                    parseString parseFraction mempty
            let virtuousFraction' =
                    parseString virtuousFraction mempty
            parseFraction' shouldWork
                `shouldBe` Success (1 % 2)
            parseFraction' shouldAlsoWork
                `shouldBe` Success (2 % 1)
            {- show (parseFraction' badFraction) -}
                {- `shouldBe` "Success *** Exception: Ratio has zero denominator" -}
            virtuousFraction' shouldWork `shouldBe`
                Success (1 % 2)
            virtuousFraction' shouldAlsoWork `shouldBe`
                Success (2 % 1)
        it "pulls integers as well" $ do
            parseString integer mempty "123abc"
                `shouldBe` Success 123
            parseString (integer >> eof) mempty "123"
                `shouldBe` Success ()
        it "could utilizie Alternative parsing" $ do
            let p f i = parseString f mempty i
            p (some letter) a `shouldBe` Success "blah"
            p integer b `shouldBe` Success 123
