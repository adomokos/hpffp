module CH20.DerivedOperationsSpec where

import Test.Hspec
import Data.Foldable

main :: IO ()
main = hspec spec

{-
  toList :: t a -> [a]

  -- tests wether a list is empty or not
  null :: t a -> Bool

  -- how many `a` values inhabit the t a
  -- tuple's first argument is part of t, not part of `a`
  length :: t a -> Int

  -- Doe the element occur in the structure
  elem :: Eq a => a -> t a -> Bool

  -- Largest element in a list
  maximum :: Ord a => t a -> a

  -- Least element in a list
  minimum :: Ord a => t a -> a

  sum :: (Foldable t, Num a) => t a -> a
  product :: (Foldable t, Num a) => t a -> a
-}

spec :: Spec
spec = do
  describe "Some basic derived operations" $ do
    context "toList" $ do
      it "converts to a list with toList" $ do
        toList (Just 1) `shouldBe` [1]
        let xs = [Just 1, Just 2, Just 3]
        map toList xs `shouldBe` [[1],[2],[3]]
        concatMap toList xs `shouldBe` [1,2,3]
      it "leaves out Nothing" $ do
        let xs = [Just 1, Just 2, Nothing]
        concatMap toList xs `shouldBe` [1,2]
      it "applies lst element of tuple with toList" $ do
        toList (1,2) `shouldBe` [2]
    context "null" $
      it "works with various data types" $ do
        null (Left 3) `shouldBe` True
        null [] `shouldBe` True
        null Nothing `shouldBe` True
        null (Just 3) `shouldBe` False
        null (1,2) `shouldBe` False
        let xs = [Just 1, Just 2, Nothing]
        fmap null xs `shouldBe` [False, False, True]
    context "length" $ do
      it "works with various data types" $ do
        length (Just [1,2,3]) `shouldBe` 1
        fmap length (Just [1,2,3]) `shouldBe` Just 3
        let xs = [Just 1, Just 2, Just 3]
        fmap length xs `shouldBe` [1,1,1]
        let xs' = [Just 1, Just 2, Nothing]
        fmap length xs' `shouldBe` [1,1,0]
    context "elem" $ do
      it "works with various data types" $ do
        elem 2 (Just 3) `shouldBe` False
        elem 2 (Just 2) `shouldBe` True
        elem True (Left False) `shouldBe` False
        elem True (Left True) `shouldBe` False
        elem True (Right False) `shouldBe` False
        elem True (Right True) `shouldBe` True
        let xs = [Right 1, Right 2, Right 3]
        fmap (elem 3) xs `shouldBe` [False, False, True]
      context "maximum" $ do
        it "works with various data types" $ do
          maximum [10, 12, 33, 5] `shouldBe` 33
          let xs = [Just 2, Just 10, Just 4]
          fmap maximum xs `shouldBe` [2,10,4]
          fmap maximum (Just [3, 7, 10, 2])
            `shouldBe` Just 10
          minimum "hello" `shouldBe` 'e'
