module Hpffp.RomanNumeralsSpec where

import Test.Hspec
import Test.QuickCheck
import Data.Map (Map)

type Alpha = Int
type Roman = String
type Conversions = Map Alpha Roman

main :: IO ()
main = hspec spec

convertAlpha :: Integer -> [Char]
convertAlpha 0 = []
convertAlpha x
  | x >= 10 = "X" ++ convertAlpha (x-10)
  | x == 9 = "IX" ++ convertAlpha (x-9)
  | x >= 5 = "V" ++ convertAlpha (x-5)
  | x == 4 = "IV" ++ convertAlpha (x-4)
  | otherwise = "I" ++ convertAlpha (x -1)

spec :: Spec
spec = do
  describe "Roman Numerals Conversion" $ do
    it "converts 1 to I" $ do
      convertAlpha 1 `shouldBe` "I"
    it "converts 2 to II" $ do
      convertAlpha 2 `shouldBe` "II"
    it "converts 3 to III" $ do
      convertAlpha 3 `shouldBe` "III"
    it "converts 4 to IV" $ do
      convertAlpha 4 `shouldBe` "IV"
    it "converts 5 to V" $ do
      convertAlpha 5 `shouldBe` "V"
    it "converts 6 to VI" $ do
      convertAlpha 6 `shouldBe` "VI"
    it "converts 8 to VIII" $ do
      convertAlpha 8 `shouldBe` "VIII"
    it "converts 9 to IX" $ do
      convertAlpha 9 `shouldBe` "IX"
    it "converts 10 to X" $ do
      convertAlpha 10 `shouldBe` "X"
    it "converts 11 to XI" $ do
      convertAlpha 11 `shouldBe` "XI"
