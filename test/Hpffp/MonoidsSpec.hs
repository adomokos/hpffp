module Hpffp.MonoidsSpec where

import Test.Hspec
import Test.QuickCheck
import Data.Monoid

main :: IO ()
main = hspec spec

data Optional a =
    Nada
  | Only a
  deriving (Show, Eq)

instance Monoid a => Monoid (Optional a) where
    mempty = Nada

    mappend Nada (Only x) = Only x
    mappend (Only x) Nada = Only x
    mappend (Only x) (Only y) = Only (x `mappend` y)

spec :: Spec
spec = do
    describe "Number Monoids" $ do
        it "works with Sum and Product types" $ do
            let result = mappend (Sum 1) (Sum 5)
            getSum result `shouldBe` 6
            let result = mappend (Product 5) (Product 5)
            getProduct result `shouldBe` 25
            let result = mappend (Sum 4.5) (Sum 3.4)
            getSum  result `shouldBe` 7.9
        it "can join multiple items" $ do
            let result = mappend mempty Sum 9
            getSum result `shouldBe` 9
            let result = (Sum 8) <> (Sum 9) <> (Sum 10)
            getSum result `shouldBe` 27
            let result = mconcat [(Sum 8), (Sum 9), (Sum 10)]
            getSum result `shouldBe` 27
        it "can be used in folds" $ do
            let result = foldr mappend mempty ([2, 4, 6] :: [Product Int])
            getProduct result `shouldBe` 48
            let result = foldr mappend mempty ([2, 4, 6] :: [Sum Int])
            getSum result `shouldBe` 12
            let result = foldr mappend mempty ["blah", "woot"]
            result `shouldBe` "blahwoot"
    describe "Boolean Monoids" $ do
        it "defines All" $ do
            let result = All True <> All True
            getAll result `shouldBe` True
            let result = All True <> All False
            getAll result `shouldBe` False
        it "defines Any" $ do
            let result = Any True <> Any False
            getAny result `shouldBe` True
            let result = Any False <> Any False
            getAny result `shouldBe` False
    describe "Maybe Monoids" $ do
        it "defines First" $ do
            let result = First (Just 1) `mappend` First (Just 2)
            getFirst result `shouldBe` Just 1
            let result = First Nothing <> First (Just 2)
            getFirst result `shouldBe` Just 2
            let result = First Nothing <> First Nothing
            getFirst result `shouldBe` (Nothing :: Maybe Int)
        it "defines Last" $ do
            let result = Last (Just 1) `mappend` Last (Just 2)
            getLast result `shouldBe` Just 2
            let result = Last (Just 1) <> Last Nothing
            getLast result `shouldBe` Just 1
            let result = Last Nothing <> Last Nothing
            getLast result `shouldBe` (Nothing :: Maybe Int)
    describe "Optional with Monoids" $ do
        it "works with monoid operations" $ do
            let (Only result) = Only (Sum 1) `mappend` Only (Sum 1)
            getSum result `shouldBe` 2
            let (Only result) = Only (Sum 1) <> Nada
            getSum result `shouldBe` 1
            let (Only result) = Only [1] `mappend` Nada
            result `shouldBe` [1]
            let (Only result) = Nada `mappend` Only (Sum 1)
            getSum result `shouldBe` 1
    describe "Identity" $ do
        let myList = [1..424]
        it "uses 0 for identity for addition" $
            map (+0) myList `shouldBe` myList
        it "can't use 0 for identity for multiplication" $
            map (*0) myList `shouldNotBe` myList
        it "uses 1 for identity for multiplication" $
            map (*1) myList `shouldBe` myList
        it "can't use 1 for identity for addition" $
            map (+1) myList `shouldNotBe` myList
