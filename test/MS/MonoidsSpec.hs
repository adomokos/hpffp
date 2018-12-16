module MS.MonoidsSpec where

{-
  **Monoid is a binary associative operation with an identity.**

  `mempty` from the `Monoid` typeclass is a generic identity value.
  mappend x mempty = x
  mappend mempty x = x

  In plain English, a monoid is a function that takes two arguments and follows
  two laws: associativity and identity. Associativity means the arguments can
  be regrouped. Identity means there exists some value that when we pass it as
  input to our function, the operation rendered moot and the other value is returned.

  Monoid the typeclass that generalizes these laws across types.

  The typeclass Monoid is defined:

  class Monoid m where
    mempty :: m
    mappend :: m -> m -> m
    mconcat :: [m] -> m
    mconcat = foldr mappend mempty
-}

import Test.Hspec
import Data.Monoid

main :: IO ()
main = hspec spec

data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  Only x `mappend` Nada = Only x
  Nada `mappend` Only x = Only x
  Only x `mappend` Only y = Only (x <> y)

newtype First' a = First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = First' Nada
  mappend (First' Nada) (First' Nada) = First' Nada
  mappend (First' Nada) (First' (Only x)) = First' (Only x)
  mappend (First' (Only x)) (First' Nada) = First' (Only x)
  mappend (First' (Only x)) (First' (Only _)) = First' (Only x)

spec :: Spec
spec = do
  describe "Monoids" $ do
    it "can use mappend to combine lists" $ do
      mappend [1,2,3] [4,5,6] `shouldBe` [1..6]
      mappend [1,2,3] [] `shouldBe` [1..3]
      mappend [] [1,2,3] `shouldBe` [1..3]
    it "can combine lists" $ do
      mconcat [[1..3], [4..6]] `shouldBe` [1..6]
      mappend "Trout" " goes well with garlic"
        `shouldBe` "Trout goes well with garlic"
    it "is similar to addition with lists" $ do
      (++) [1..3] [4..6] `shouldBe` [1..6]
      (++) "Trout" " goes well with garlic"
        `shouldBe` "Trout goes well with garlic"
      foldr (++) [] [[1..3], [4..6]]
        `shouldBe` [1..6]
      foldr mappend mempty [[1..3], [4..6]]
        `shouldBe` [1..6]
    it "uses Sum and Product for numbers" $ do
      mappend (Sum 1) (Sum 2) `shouldBe` Sum 3
      mappend (Product 2) (Product 3) `shouldBe` Product 6
      mappend (Sum 4.5) (Sum 3.4) `shouldBe` Sum 7.9
    it "can do more with Sum and Product" $ do
      mappend mempty (Sum 3) `shouldBe` Sum 3
      Sum 3 <> Sum 4 <> Sum 5 `shouldBe` Sum 12
      mconcat [Sum 8, Sum 9, Sum 10] `shouldBe` Sum 27
    it "can use Product to calculate list sum" $ do
      foldr mappend mempty ([2,4,6] :: [Product Int])
        `shouldBe` Product 48
      foldr mappend mempty ([2,4,6] :: [Sum Int])
        `shouldBe` Sum 12
  describe "Monoid Laws" $ do
    it "has left identity" $ do
      mappend mempty [1,2] `shouldBe` [1,2]
      mappend mempty (Sum 2) `shouldBe` Sum 2
    it "has right identity" $ do
      mappend [1,2] mempty `shouldBe` [1,2]
      mappend (Sum 2) mempty `shouldBe` Sum 2
    it "has associtativity" $ do
      mappend [1,2] (mappend [2,3] [3,4])
        `shouldBe` mappend (mappend [1,2] [2,3]) [3,4]
      mappend (Sum 2) (mappend (Sum 3) (Sum 4))
        `shouldBe` mappend (mappend (Sum 2) (Sum 3)) (Sum 4)
      Sum 2 <> (Sum 3 <> Sum 4) `shouldBe` (Sum 2 <> Sum 3) <> Sum 4
  describe "Different instances" $ do
    it "works with All" $ do
      All True <> All True `shouldBe` All True
      All True <> All False `shouldBe` All False
    it "works with Any" $ do
      Any True <> Any True `shouldBe` Any True
      Any True <> Any False `shouldBe` Any True
      Any False <> Any False `shouldBe` Any False
  describe "Optional Monoid" $ do
    it "works with custom Maybe type" $ do
      Only (Sum 1) <> Only (Sum 2)
        `shouldBe` Only (Sum 3)
      Only (Product 2) <> Only (Product 4)
        `shouldBe` Only (Product 8)
      Only (Sum 2) <> Nada
        `shouldBe` Only (Sum 2)
      Only [1] <> Nada `shouldBe` Only [1]
      Nada <> Only (Sum 1) `shouldBe` Only (Sum 1)
    it "works with newtype First'" $ do
      First' (Only 1) `mappend` First' Nada
        `shouldBe` First' (Only 1)
      First' Nada `mappend` First' Nada
        `shouldBe` (First' Nada :: First' [Int])
      First' Nada `mappend` First' (Only 2)
        `shouldBe` (First' (Only 2))
      First' (Only 1) `mappend` First' (Only 2)
        `shouldBe` First' (Only 1)
