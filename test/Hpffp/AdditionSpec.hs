module Hpffp.AdditionSpec where

import Test.Hspec

divideBy :: Integral a => a -> a -> (a, a)
divideBy num denom = go num denom 0
    where go n d count
            | n < d = (count, n)
            | otherwise =
                go (n - d) d (count + 1)

myMultiply :: (Eq a, Num a) => a -> a -> a
myMultiply x 0 = 0
myMultiply x 1 = x
myMultiply x n = x + myMultiply x (n -1)

main :: IO ()
main = hspec $ do
    describe "Division" $ do
        it "15 divided by 3 is 5" $ do
            divideBy 15 3 `shouldBe` (5, 0)
        it "22 divided by 5 is 4 remainder 2" $ do
            divideBy 22 5 `shouldBe` (4 ,2)
    describe "Addition" $ do
        it "1 + 1 is greater than 1" $ do
            (1 + 1) > 1 `shouldBe` True
        it "2 + 2 is equal to 4" $ do
            2 + 2 `shouldBe` 4
    describe "Multiply" $ do
        it "multiplies 5 by 3" $ do
            myMultiply 5 3 `shouldBe` 15
        it "multiplies 5 by 0" $ do
            myMultiply 5 0 `shouldBe` 0

