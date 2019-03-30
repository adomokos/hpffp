-- {-# OPTIONS_GHC -fno-warn-type-defaults #-}
module CH20.FoldableIntroSpec where

import Test.Hspec
import Data.Foldable
import Data.Monoid (Sum(..), Product(..), All(..), Any(..)
                   ,First(..), Last(..))

newtype MySum = MySum Int deriving (Eq, Show)

instance Semigroup MySum where
  (MySum x) <> (MySum y) = MySum (x+y)

instance Monoid MySum where
  mempty = MySum 0

{-
  class Foldable (t :: * -> *) where
    fold :: Monoid m => t m -> m
    foldMap :: Monoid m
            => (a -> m) -> t a -> m

  Foldable is a way of generalizing catamorphisms - folding -
  to different datatypes, it forces you to think about the monoid
  you're using to combine values.
-}

main :: IO ()
main = hspec spec

myxs :: [Sum Integer]
myxs = [1,2,3,4,5]

myys :: [Product Integer]
myys = [1,2,3,4,5]

spec :: Spec
spec =
  describe "Foldable Intro" $ do
    it "relates to Monoids" $ do
      (MySum 3 `mappend` mempty) `shouldBe` MySum 3
      (MySum 3 `mappend` MySum 5) `shouldBe` MySum 8
    it "can be observed in a very basic operation" $ do
      foldr (+) 0 ([1..5] :: [Int]) `shouldBe` 15
      sum ([1..5] :: [Int]) `shouldBe` 15

    context "fold" $ do
      it "can fold over Monoid lists" $ do
        fold (map Sum [1..5]) `shouldBe` Sum 15
        fold myxs `shouldBe` Sum 15
        fold myys `shouldBe` Product 120
        fold ["hello", "world"] `shouldBe` "helloworld"

    context "foldMap" $ do
      it "uses a function in foldMap" $ do
        foldMap Sum [1,2,3,4] `shouldBe` Sum 10
        foldMap Product [1,2,3,4] `shouldBe` Product 24
        foldMap All [True, False, True] `shouldBe` All False
        foldMap Any [(3==4), (4<5)] `shouldBe` Any True
        foldMap First [Just 1, Nothing, Just 5]
          `shouldBe` First (Just 1)
        foldMap Last [Just 1, Nothing, Just 5]
          `shouldBe` Last (Just 5)
      it "recognizes foldMap with a Monoid" $
        foldMap MySum [1,2,3,4] `shouldBe` MySum 10
      it "can use regular functions with it" $ do
        let products = map Product [1..3]
        foldMap (*5) products `shouldBe` Product 750
        let sums = map Sum [1..3]
        foldMap (*5) sums `shouldBe` Sum 30
      it "will not change the foldMap behavior with 1 instance" $ do
        let result = foldMap (*5) (Just 100) :: Product Integer
            result2 = foldMap (*5) (Just 5) :: Sum Integer
        result `shouldBe` 500
        result2 `shouldBe` Sum 25
      it "uses mempty when the fold subject is empty" $ do
        let result = foldMap (*5) Nothing :: Sum Integer
            result2 = foldMap (*5) Nothing :: Product Integer
        result `shouldBe` Sum 0
        result2 `shouldBe` Product 1

    context "foldr" $ do
      it "has the Monoid instance baked in" $ do
        foldr (*) 5 [1,2,3] `shouldBe` 30
      it "ignores the Monoid instance" $ do
        let sumXs = map Sum [2..4]
        foldr (*) 3 sumXs `shouldBe` Sum 72
        let productXs = map Product [2..4]
        foldr (*) 3 productXs `shouldBe` Product 72

