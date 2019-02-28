module CH21.TraversableIntroSpec where

import Test.Hspec

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
