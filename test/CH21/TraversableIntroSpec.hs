module CH21.TraversableIntroSpec where

import Test.Hspec

import Data.Maybe (catMaybes)

main :: IO ()
main = hspec spec

{-
  class (Functor t, Foldable t)
    => Traversable t where
    traverse :: Applicative f
             => (a -> f b)
             -> t a
             -> f (t b)
    traverse f = sequenceA . fmap f

  fmap     ::     Functor f => (a -> f b) -> f a -> f b
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

  -- The counterpart to traverse is sequenceA
  -- | Evalute each action in the structure from left to right,
  --   and collect the results.
  sequenceA :: Applicative f => t (f a) -> f (t a)
  sequenceA = traverse id
  {-# MINIMAL traverse | sequenceA #-}
-}

spec :: Spec
spec = do
  describe "Traversable Intro" $ do
    it "compare these:" $ do
      sum [1,2,3] `shouldBe` 6
      fmap sum [Just 1, Just 2, Just 3]
        `shouldBe` [1,2,3]
      (fmap . fmap) sum Just [1,2,3]
        `shouldBe` Just 6
      let xs = [Just 1, Just 2, Nothing]
      fmap product xs `shouldBe` [1,2,1]
    it "to these: " $ do
      fmap Just [1,2,3] `shouldBe` [Just 1, Just 2, Just 3]
      (sequenceA $ fmap Just [1,2,3])
        `shouldBe` Just [1,2,3]
      let xs = [Just 1, Just 2, Just 3]
      sequenceA xs `shouldBe` Just [1,2,3]
      let xsn = [Just 1, Just 2, Nothing]
      sequenceA xsn `shouldBe` Nothing
      (fmap sum $ sequenceA xs)
        `shouldBe` Just 6
      (fmap product $ sequenceA xsn)
        `shouldBe` Nothing
    it "note catMaybe from Data.Maybe" $ do
      let xs = [Just 1, Just 2, Just 3]
      let xsn = [Just 1, Just 2, Nothing]
      catMaybes xs `shouldBe` [1..3]
      catMaybes xsn `shouldBe` [1,2]
      let xsn' = xs ++ [Nothing]
      (sum $ catMaybes xsn') `shouldBe` 6
      (fmap sum $ sequenceA xsn')
        `shouldBe` Nothing
