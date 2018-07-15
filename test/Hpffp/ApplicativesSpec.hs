module Hpffp.ApplicativesSpec where

import Test.Hspec
import Data.Monoid
import Control.Applicative
import Data.Char (toUpper)
import Data.List (elemIndex)

main :: IO ()
main = hspec spec

{-
     ($)  ::   (a -> b) ->   a ->   b
    (<$>) ::   (a -> b) -> f a -> f b
    (<*>) :: f (a -> b) -> f a -> f b
-}

type Id = Identity

f x =
    lookup x [ (3, "hello")
             , (4, "julie")
             , (5, "kbai")]

g y =
    lookup y [ (7, "sup?")
             , (8, "chris")
             , (9, "aloha")]

h z =
    lookup z [(2,3), (5,6), (7,8)]
m x =
    lookup x [(4,10), (8,13), (1, 9001)]

-- Exercises
-- 1.
added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1,2,3] [4,5,6])

-- 2.
y :: Maybe Integer
y = lookup 3 $ zip [1,2,3] [4,5,6]

z :: Maybe Integer
z = lookup 2 $ zip [1,2,3] [4,5,6]

tupled  :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

-- 3.
x' :: Maybe Int
x' = elemIndex 3 [1,2,3,4,5]

y' :: Maybe Int
y' = elemIndex 4 [1,2,3,4,5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x' <*> y'

-- 4.
xs :: [Integer]
xs = [1,2,3]

ys :: [Integer]
ys = [4,5,6]

x'' :: Maybe Integer
x'' = lookup 3 $ zip xs ys

y'' :: Maybe Integer
y'' = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = fmap sum $ (,) <$> x'' <*> y''

newtype Identity a = Identity a
    deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
    pure = Identity
    Identity f <*> Identity x = Identity (f x)

spec :: Spec
spec = do
    describe "helps with Map lookup" $ do
        it "finds values with tie-fighter" $ do
            f 3 `shouldBe` Just "hello"
            g 8 `shouldBe` Just "chris"
            (++) <$> f 3 <*> g 7
                `shouldBe` Just "hellosup?"
            (+) <$> h 5 <*> m 1
                `shouldBe` Just 9007
            (+) <$> h 5 <*> m 6 `shouldBe` Nothing
        it "finds values with liftA2" $ do
            liftA2 (++) (g 9) (f 4)
                `shouldBe` Just "alohajulie"
            liftA2 (^) (h 5) (m 4)
                `shouldBe` Just 60466176
            liftA2 (*) (h 5) (m 4) `shouldBe` Just 60
            liftA2 (*) (h 1) (m 1) `shouldBe` Nothing
    describe "Applicatives" $ do
        it "is a layer on top of Functors" $ do
            fmap (+1) [1..3] `shouldBe` [2..4]
            pure (+1) <*> [1..3] `shouldBe` [2..4]
        it "is a Monoid structure and function application" $
            [(*2), (*3)] <*> [4,5] `shouldBe` [8,10,12,15]
        it "can enrich Maybe values" $ do
            Just (*2) <*> Just 4 `shouldBe` Just 8
            Just (*2) <*> Nothing `shouldBe` Nothing
            Nothing <*> Just 2 `shouldBe` (Nothing :: Maybe Int)
            Nothing <*> Nothing `shouldBe` (Nothing :: Maybe Int)
        it "can work with monoids" $ do
            fmap (+1) ("blah",0) `shouldBe` ("blah",1)
            ("Woo", (+1)) <*> (" Hoo!", 0)
                `shouldBe` ("Woo Hoo!", 1)
            (Sum 2, (+1)) <*> (Sum 1, 0)
                `shouldBe` (Sum 3, 1)
            (Product 3, (+9)) <*> (Product 2, 8)
                `shouldBe` (Product 6, 17)
            (All True, (+1)) <*> (All False, 0)
                `shouldBe` (All False, 1)
        it "maps a single function over a plurality of values with Functor" $ do
            fmap (2^) [1,2,3] `shouldBe` [2,4,8]
            fmap (^2) [1,2,3] `shouldBe` [1,4,9]
        it "maps plurality of functions over plurality of values" $
            [(+1),(*2)] <*> [2,4] `shouldBe` [3,5,4,8]
        it "maps the function over lists" $ do
            (,) <$> [1,2] <*> [3,4]
                `shouldBe` [(1,3),(1,4),(2,3),(2,4)]
            liftA2 (,) [1,2] [3,4]
                `shouldBe` [(1,3),(1,4),(2,3),(2,4)]
            (+) <$> [1,2] <*> [3,5]
                `shouldBe` [4,6,5,7]
            liftA2 (+) [1,2] [3,5]
                `shouldBe` [4,6,5,7]
            max <$> [1,2] <*> [1,4]
                `shouldBe` [1,4,2,4]
            liftA2 (max) [1,2] [1,4]
                `shouldBe` [1,4,2,4]
        it "can be used for some useful examples" $ do
            let l= lookup 3 [(3, "hello")]
            l `shouldBe` Just "hello"
            fmap length l `shouldBe` Just 5
            let c (x:xs) = toUpper x:xs
            fmap c l `shouldBe` Just "Hello"
            fmap length l `shouldBe` Just 5
    describe "Exercises" $ do
        it "verifies them" $ do
            added `shouldBe` Just 9
            tupled `shouldBe` Just (6,5)
            maxed `shouldBe` Just 3
    describe "Identity" $ do
        it "provides a thin wrapper context" $ do
            let xs = [1,2,3]
                ys = [9,9,9]
            const <$> xs <*> ys
                `shouldBe` [1,1,1,2,2,2,3,3,3]
            let mkId = Identity
            const <$> mkId xs <*> mkId ys
                `shouldBe` Identity [1,2,3]
