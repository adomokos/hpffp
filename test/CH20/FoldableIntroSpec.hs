-- {-# OPTIONS_GHC -fno-warn-type-defaults #-}
module CH20.FoldableIntroSpec where

import Test.Hspec
import Data.Foldable
import Data.Monoid (Sum(..))

newtype MySum = MySum Int deriving (Eq, Show)

instance Monoid MySum where
  mempty = MySum 0
  mappend (MySum x) (MySum y) = MySum (x+y)

{-
  class Foldable (t :: * -> *) where
    fold :: Monoid m => t m -> m
    foldMap :: Monoid m
            => (a -> m) -> t a -> m
-}

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Foldable Intro" $ do
    it "relates to Monoids" $ do
      (MySum 3 `mappend` mempty) `shouldBe` MySum 3
      (MySum 3 `mappend` MySum 5) `shouldBe` MySum 8
    it "can be observed in a very basic operation" $ do
      foldr (+) 0 ([1..5] :: [Int]) `shouldBe` 15
      sum ([1..5] :: [Int]) `shouldBe` 15
    it "can fold over Monoid lists" $ do
      fold (map Sum [1..5])`shouldBe` Sum 15
