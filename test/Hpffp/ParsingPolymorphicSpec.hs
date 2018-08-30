{-# LANGUAGE OverloadedStrings #-}
module Hpffp.ParsingPolymorphicSpec where

import Control.Applicative
import Data.Attoparsec.Text (parseOnly)
import Data.Ratio ((%))
import Data.String (IsString)
import Text.Trifecta
import Test.Hspec

main :: IO ()
main = hspec spec

badFraction :: IsString s => s
badFraction = "1/0"

alsoBad :: IsString s => s
alsoBad = "10"

shouldWork :: IsString s => s
shouldWork = "1/2"

shouldAlsoWork :: IsString s => s
shouldAlsoWork = "2/1"

parseFraction :: (Monad m, TokenParsing m)
              => m Rational
parseFraction = do
    numerator <- decimal
    _ <- char '/'
    denominator <- decimal
    case denominator of
      0 -> fail "Denominator cannot be zero"
      _ -> return (numerator % denominator)

spec :: Spec
spec = do
    describe "Polymorphic parsing" $ do
        it "parses with Attoparsec" $ do
            let attoP = parseOnly parseFraction
                (Left x) = attoP badFraction
            x `shouldContain` "Failed reading"
            let (Right y) = attoP shouldWork
            y `shouldBe` (1 % 2)
            let (Right y) = attoP shouldAlsoWork
            y `shouldBe` (2 % 1)
            let (Left x) = attoP alsoBad
            x `shouldBe` "\"/\": not enough input"

        it "parses with Trifecta" $ do
            let p f i = parseString f mempty i
                (Failure x) = p parseFraction badFraction
            show x `shouldContain` "cannot be zero" -- meh for `show`
            let (Success y) = p parseFraction shouldWork
            y `shouldBe` (1 % 2)
            let (Success y) = p parseFraction shouldAlsoWork
            y `shouldBe` (2 % 1)
            let (Failure (ErrInfo x y)) = p parseFraction alsoBad
            show x `shouldContain` "unexpected" -- meh for `show`
