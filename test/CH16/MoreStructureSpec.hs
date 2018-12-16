{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
module CH16.MoreStructureSpec where

import Test.Hspec

main :: IO ()
main = hspec spec

-- f is a function
data Wrap f a =
  Wrap (f a)
  deriving (Show, Eq)

instance Functor f => Functor (Wrap f) where
  fmap f (Wrap fa) = Wrap (fmap f fa)

provideLine :: IO String
provideLine = pure "[1,2,3]"

provideInt :: IO Int
provideInt = pure 10

{-
   fmap :: (a -> b) -> f a -> f b
   -- contrast this to
   nat :: (f -> g)  -> f a -> g a
   f and g in (f -> g) are higher-kinded types.
-}

type Nat f g = forall a . f a -> g a

-- This will work
maybeToList :: Nat Maybe []
maybeToList Nothing = []
maybeToList (Just a) = [a]

{-
-- This will not work, not allowed
degenerateMtl :: Nat Maybe []
degenerateMtl Nothing = []
degenerateMtl (Just a) = [a+1]
-}

data Tuple a b =
  Tuple a b
  deriving (Show, Eq)

newtype Flip f a b =
  Flip (f b a)
  deriving (Show, Eq)

-- this works, goofy as it looks
instance Functor (Flip Tuple a) where
  fmap f (Flip (Tuple a b)) =
    Flip $ Tuple (f a) b

spec :: Spec
spec = do
  describe "16.13 More structures" $ do
    it "works with wrapper types" $ do
      let x = Wrap (Just 1)
      fmap (+1) x `shouldBe` Wrap (Just 2)
      let y = Wrap [1,2,3]
      fmap (+1) y `shouldBe` Wrap [2,3,4]
  describe "16.14 IO Functor" $ do
    it "works with IO" $ do
      result <- fmap read provideLine :: IO [Int]
      result `shouldBe` [1,2,3]
      result' <- fmap (+1) provideInt
      result' `shouldBe` 11
  describe "16.15 Doing something different" $ do
    it "works with natural transformation" $ do
      let result = maybeToList Nothing :: [Int]
      result `shouldBe` []
      let result' = maybeToList (Just 2)
      result' `shouldBe` [2]
  describe "16.16 Functors are unique to a datatype" $ do
    it "needs flip for Tuple" $ do
      let t = Tuple 1 "blah"
      fmap (+1) (Flip t) `shouldBe` Flip (Tuple 2 "blah")
