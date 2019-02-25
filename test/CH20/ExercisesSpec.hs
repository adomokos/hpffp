module CH20.ExercisesSpec where

import Test.Hspec
import Data.Monoid(Sum(..), Product(..))

main :: IO ()
main = hspec spec

sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x = foldr (\y acc -> (x == y) || acc) False

minimum' :: (Foldable t, Ord a) => t a -> a
minimum' = foldr1 (\x acc -> if x < acc then x else acc)

maximum' :: (Foldable t, Ord a) => t a -> a
maximum' = foldr1 (\x acc -> if x > acc then x else acc)

null' :: (Foldable t) => t a -> Bool
null' = foldr (\_ _ -> False) True

length' :: (Foldable t) => t a -> Int
length' = foldr (\_ acc -> acc + 1) 0

toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) []
-- toList' = foldMap id

fold' :: (Foldable t, Monoid a) => t a -> a
fold' = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (mappend . f) mempty

-- Chapter Exercises
-- 1.
newtype Constant a b =
  Constant b deriving (Show, Eq)

instance Foldable (Constant a) where
  foldr f z (Constant x) = f x z
  foldl f z (Constant x) = f z x
  foldMap f (Constant x) = f x

-- 2.
data Two a b = Two a b deriving (Show, Eq)

instance Foldable (Two a) where
  foldr f z (Two _ y) = f y z
  foldl f z (Two _ y) = f z y
  foldMap f (Two _ y) = f y

-- 3.
data Three a b c = Three a b c deriving (Show, Eq)

instance Foldable (Three a b) where
  foldr f s (Three _ _ z) = f z s
  foldl f s (Three _ _ z) = f s z
  foldMap f (Three _ _ z) = f z

-- 4.
data Three' a b = Three' a b b deriving (Show, Eq)

instance Foldable (Three' a) where
  foldMap f (Three' _ x y) = mappend (f x) (f y)

-- 5.
data Four a b = Four a b b b deriving (Show, Eq)

instance Foldable (Four a) where
  foldMap f (Four _ k l m) =
    (f k) `mappend` (f l) `mappend` (f m)

-- 6.
filterF :: ( Applicative f
           , Foldable t
           , Monoid (f a)
           ) => (a -> Bool) -> t a -> f a
filterF f = foldMap (\a -> if f a then pure a else mempty)

spec :: Spec
spec = do
  describe "Exercises" $ do
    it "works with custom sum" $ do
      sum' [1..4] `shouldBe` 10
      sum' [] `shouldBe` 0
    it "works with custom product" $ do
      product' [] `shouldBe` 1
      product' [1,2,3,4] `shouldBe` 24
    it "works with custom elem'" $ do
      elem' 3 [] `shouldBe` False
      elem' 3 [1,2,3,4] `shouldBe` True
    it "works with custom minimum'" $ do
      minimum' [1] `shouldBe` 1
      minimum' [2,3,1,4] `shouldBe` 1
    it "works with custom maximum'" $ do
      maximum' [1] `shouldBe` 1
      maximum' [2,3,1,4] `shouldBe` 4
    it "works with custom null'" $ do
      null' [1] `shouldBe` False
      null' [] `shouldBe` True
    it "works with custom length'" $ do
      length' [1,2,3,4] `shouldBe` 4
      length' [] `shouldBe` 0
      length' Nothing `shouldBe` 0
      length' (Just 1) `shouldBe` 1
    it "works with custom toList'" $ do
      toList' (Just 1 :: Maybe Int) `shouldBe` [1]
      toList' (Nothing :: Maybe Int) `shouldBe` []
      toList' [1,2,3] `shouldBe` [1,2,3]
    it "works with custom fold'" $ do
      fold' [Sum 1, Sum 2] `shouldBe` 3
      fold' ([] :: [Sum Int]) `shouldBe` 0
    it "works with custom foldMap'" $ do
      foldMap' Sum [1,2,3] `shouldBe` 6
      foldMap' Sum [] `shouldBe` 0

  describe "Chapter Exercises" $ do
    it "has Foldable for Constant" $ do
      foldr (*) 2 (Constant 12) `shouldBe` 24
      foldl (*) 2 (Constant 12) `shouldBe` 24
      foldMap (*2) (Constant (Sum 12)) `shouldBe` 24
    it "has Foldable for Two" $ do
      foldr (*) 2 (Two "Hey" 23) `shouldBe` 46
      foldl (*) 2 (Two "Hey" 23) `shouldBe` 46
      foldMap (+2) (Two "Hey" (Sum 12)) `shouldBe` 14
    it "has Foldable for Three" $ do
      foldr (*) 2 (Three "Hey" True 23) `shouldBe` 46
      foldl (*) 2 (Three "Hey" True 23) `shouldBe` 46
      foldMap (+2) (Three "Hey" False (Sum 12)) `shouldBe` 14
    it "has Foldable for Three'" $
      foldMap (+2) (Three' "Hey" (Sum 23) (Sum 12)) `shouldBe` 39
    it "has Foldable for Four" $
      foldMap (+2) (Four "Hey" (Sum 1) (Sum 2) (Sum 3))
        `shouldBe` 12
    it "works with filterF" $ do
      filterF (>2) [Sum 1, Sum 2, Sum 3]
        `shouldBe` [Sum 3]
      filterF (==5) [Sum 1, Sum 2, Sum 3]
        `shouldBe` []
