module Hpffp.AdditionSpec where

import Test.Hspec
import Test.QuickCheck

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

trivialInt :: Gen Int
trivialInt = return 1

oneThroughThree :: Gen Int
oneThroughThree = elements [1,2,2,2,2,3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
    a <- arbitrary
    b <- arbitrary
    return (a, b)

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genThreeple = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (a, b, c)

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
    a <- arbitrary
    b <- arbitrary
    elements [Left a, Right b]

-- equal probability
genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
    a <- arbitrary
    elements [Nothing, Just a]

-- What QuickCheck does so you get more Just values
genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
    a <- arbitrary
    frequency [ (1, return Nothing)
              , (3, return (Just a))]

main :: IO ()
main = hspec $ do
    describe "Division" $ do
        it "15 divided by 3 is 5" $ do
            divideBy 15 3 `shouldBe` (5, 0)
        it "22 divided by 5 is 4 remainder 2" $ do
            divideBy 22 5 `shouldBe` (4 ,2)
    describe "Multiply" $ do
        it "multiplies 5 by 3" $ do
            myMultiply 5 3 `shouldBe` 15
        it "multiplies 5 by 0" $ do
            myMultiply 5 0 `shouldBe` 0
    describe "Addition" $ do
        it "1 + 1 is greater than 1" $ do
            (1 + 1) > 1 `shouldBe` True
        it "2 + 2 is equal to 4" $ do
            2 + 2 `shouldBe` 4
        it "x + 1 is always greater than x" $ do
            property $ \x -> x + 1 > (x :: Int)
