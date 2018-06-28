module Hpffp.QuickCheckSpec where

import Test.Hspec
import Test.QuickCheck
import Data.Numbers.Primes
import Data.List (sort)

half :: (Fractional a) => a -> a
half x = x / 2

main :: IO ()
main = hspec spec

prop_RevRev :: [Int] -> Bool
prop_RevRev xs = reverse (reverse xs) == xs

-- Another list testing - will fail without constraints
prop_Index_v1 :: [Integer] -> Int -> Bool
prop_Index_v1 xs n = xs !! n == head (drop n xs)

-- Improved version, but list length is not taken into account
prop_Index_v2 :: (NonEmptyList Integer) -> NonNegative Int -> Bool
prop_Index_v2 (NonEmpty xs) (NonNegative n) = xs !! n == head (drop n xs)

-- Using a constraint for n - the length of the list
prop_Index_v3 :: (NonEmptyList Integer) -> NonNegative Int -> Property
prop_Index_v3 (NonEmpty xs) (NonNegative n) =
    n < length xs ==> xs !! n == head (drop n xs)

-- Using properties
prop_Index_v4 :: (NonEmptyList Integer) -> Property
prop_Index_v4 (NonEmpty xs) =
    forAll (choose (0, length xs - 1)) $ \n -> xs !! n == head (drop n xs)

-- Reusing generator with Arbitrary
newtype Prime a = Prime a deriving Show

instance (Integral a, Arbitrary a) => Arbitrary (Prime a) where
    arbitrary = do
        x <- frequency [ (10, choose (0, 1000))
                       , (5, choose (1001, 10000))
                       , (1, choose (10001, 50000))]
        return $ Prime (primes !! x)

prop_PrimeSum_v4 :: Prime Int -> Prime Int -> Property
prop_PrimeSum_v4 (Prime p) (Prime q) =
    p > 2 && q > 2 ==> classify (p < 1000 || q < 1000) "has small prime" $ even (p + q)

-- Sorting

genList :: (Arbitrary a, Eq a) => Gen [a]
genList = do
  a <- arbitrary
  b <- arbitrary `suchThat` (/= a)
  c <- arbitrary `suchThat` (`notElem` [a, b])
  return [a, b, c]

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
    snd $ foldr go (Nothing, True) xs
        where go _ status@(_, False) = status
              go y (Nothing, t) = (Just y, t)
              go y (Just x, t) = (Just y, x >= y)

prop_listOrdered :: Property
prop_listOrdered =
    forAll (genList :: Gen String)
    (\x -> listOrdered $ sort x)

-- Addition

associative :: (Num a, Eq a) => a -> a -> a -> Bool
associative x y z = x + (y + z) == (x + y) + z

commutative :: (Num a, Eq a) => a -> a -> Bool
commutative x y = x + y == y + x

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = arbitrary

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genThreeple = arbitrary

prop_plusAssoc :: Property
prop_plusAssoc =
    forAll (genThreeple :: Gen (Int, Int, Int))
    (\(x, y, z) -> associative x y z)

prop_plusCommutative :: Property
prop_plusCommutative =
    forAll (genTuple :: Gen (Int, Int))
    (\(x, y) -> commutative x y)


spec :: Spec
spec = do
    describe "Learning QuickCheck" $ do
        it "verifies half function works" $ property $ do
            \x -> ((*2) . half) x == (x :: Rational)
        it "can reverse and reverse list" $ property prop_RevRev
        it "can verify a property of a list" $ property prop_Index_v3
        it "verifies an ordered list" $ property prop_listOrdered
        it "verifies associative function" $ property prop_plusAssoc
        it "verifies associative function" $ property prop_plusCommutative
        -- it "can verify prime numbers" $ property prop_PrimeSum_v4
