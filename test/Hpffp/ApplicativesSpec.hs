module Hpffp.ApplicativesSpec where

import Test.Hspec
import Data.Monoid
import Control.Applicative
import Data.Char (toUpper)

main :: IO ()
main = hspec spec

{-
     ($)  ::   (a -> b) ->   a ->   b
    (<$>) ::   (a -> b) -> f a -> f b
    (<*>) :: f (a -> b) -> f a -> f b
-}

spec :: Spec
spec = do
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

