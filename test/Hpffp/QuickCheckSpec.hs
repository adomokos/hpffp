module Hpffp.QuickCheckSpec where

import Test.Hspec
import Test.QuickCheck
import Data.Numbers.Primes

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

spec :: Spec
spec = do
    describe "Learning QuickCheck" $ do
        it "verifies half function works" $ property $ do
            \x -> ((*2) . half) x == (x :: Rational)
        it "can reverse and reverse list" $
            property prop_RevRev
        it "can verify a property of a list" $ property prop_Index_v3
        -- it "can verify prime numbers" $ property prop_PrimeSum_v4
