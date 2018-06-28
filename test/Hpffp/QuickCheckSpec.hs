module Hpffp.QuickCheckSpec where

import Test.Hspec
import Test.QuickCheck
import Data.Numbers.Primes
import Data.List (sort)
import Data.Char (toUpper)

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

associative :: (Eq a) => (a -> a -> a) -> a -> a -> a -> Bool
associative f x y z = x `f` (y `f` z) == (x `f` y) `f` z

commutative :: (Eq a) => (a -> a -> a) -> a -> a -> Bool
commutative f x y = x `f` y == y `f` x

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = arbitrary

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genThreeple = arbitrary

untrurry :: (a -> b -> c -> d) -> ((a, b, c) -> d)
untrurry f (a, b, c) = f a b c

prop_plusAssoc :: Property
prop_plusAssoc =
    forAll (genThreeple :: Gen (Int, Int, Int))
    (untrurry $ associative (+))

prop_plusCommutative :: Property
prop_plusCommutative =
    forAll (genTuple :: Gen (Int, Int))
    (uncurry $ commutative (+))

-- multiplication
prop_timesAssoc :: Property
prop_timesAssoc =
    forAll (genThreeple :: Gen (Int, Int, Int))
    (untrurry $ associative (*))

prop_timesCommutative :: Property
prop_timesCommutative =
    forAll (genTuple :: Gen (Int, Int))
    (uncurry $ associative (*))

-- ^
genTuplePos :: (Arbitrary a, Num a, Ord a) => Gen (a, a)
genTuplePos = do
    x <- arbitrary `suchThat` (>1)
    y <- arbitrary `suchThat` (>1)
    return (x, y)

genThreeplePos :: (Arbitrary a, Num a, Ord a) => Gen (a, a, a)
genThreeplePos = do
    x <- arbitrary `suchThat` (>1)
    y <- arbitrary `suchThat` (>1)
    z <- arbitrary `suchThat` (>1)
    return (x, y, z)

-- evauluates to false
prop_hatAssoc :: Property
prop_hatAssoc =
    forAll (genThreeplePos :: Gen (Int, Int, Int))
    (untrurry $ associative (^))

divisor :: Gen Float
divisor = arbitrary `suchThat` (/= 0)

prop_dollar :: Property
prop_dollar =
    forAll divisor
    (\x -> ((-) x $ x + x) == (-) x (x + x))

-- List function
prop_concat :: Property
prop_concat =
    forAll (genTuple :: Gen ([Int], [Int]))
    (\(x, y) -> foldr (++) [] [x, y] == concat [x, y])

prop_concat' :: Property
prop_concat' =
    forAll (genTuple :: Gen ([Int], [Int]))
    (\(x, y) -> foldr (:) y x == (++) x y)

-- Check length
prop_lengthTake :: Property
prop_lengthTake =
    forAll (genTuple :: Gen (Int, [Int]))
    (\(n, xs) -> length (take n xs) == n)

-- Show/Read
prop_showRead :: Property
prop_showRead =
    forAll (genList :: Gen String)
    (\x -> (read (show x)) == x)

twice f = f . f
fourTimes = twice . twice

capitalizeWord :: String -> String
capitalizeWord = map toUpper

prop_capitalizeWord :: Property
prop_capitalizeWord =
    forAll (genList :: Gen String)
    (\x -> capitalizeWord x == twice capitalizeWord x
           &&
           capitalizeWord x == fourTimes capitalizeWord x)

prop_sort :: Property
prop_sort =
    forAll (genList :: Gen String)
    (\x -> sort x == twice sort x
           &&
           sort x == fourTimes sort x)

data Fool =
        Fulse
      | Frue
      deriving (Eq, Show)

genFool :: Gen Fool
genFool = elements [Fulse, Frue]

genUnfair :: Gen Fool
genUnfair = elements [Fulse, Fulse, Frue]

spec :: Spec
spec = do
    describe "Learning QuickCheck" $ do
        it "verifies half function works" $ property $ do
            \x -> ((*2) . half) x == (x :: Rational)
        it "can reverse and reverse list" $ property prop_RevRev
        it "can verify a property of a list" $ property prop_Index_v3
        it "verifies an ordered list" $ property prop_listOrdered
        it "verifies associative plus function" $ property prop_plusAssoc
        it "verifies commutative plus function" $ property prop_plusCommutative
        it "verifies associative multiplies function" $ property prop_timesAssoc
        it "verifies commutative multiplies function" $ property prop_timesCommutative
        it "verifies logic for $ function" $ property prop_dollar
        it "verifies list concat function" $ property prop_concat
        it "verifies list concat function" $ property prop_concat'
        it "verifies show and read" $ property prop_showRead
        it "can capitalize words a couple of times" $ property prop_capitalizeWord
        it "can sort words a couple of times" $ property prop_sort
        -- it "verifies list length" $ property prop_lengthTake -- No...
        -- it "can verify prime numbers" $ property prop_PrimeSum_v4
