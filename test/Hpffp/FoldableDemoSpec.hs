module Hpffp.FoldableDemoSpec where

import Test.Hspec
import Data.Foldable
import Data.Monoid

main :: IO ()
main = hspec spec

data Identity a = Identity a

instance Foldable Identity where
    foldr f z (Identity x) = f x z
    foldl f z (Identity x) = f z x
    foldMap f (Identity x) = f x

data Optional a =
    Nada
  | Yep a
  deriving (Show, Eq)

instance Foldable Optional where
    foldr _ z Nada = z
    foldr f z (Yep x) = f x z

    foldl _ z Nada = z
    foldl f z (Yep x) = f z x

    foldMap _ Nada = mempty
    foldMap f (Yep a) = f a

-- Exercises:

sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x xs = getAny $ foldMap (Any . (== x)) xs

data Min a = Min { getMin :: Maybe a } deriving (Show, Eq)

instance Ord a => Monoid (Min a) where
    mempty = Min Nothing
    Min Nothing `mappend` x = x
    x `mappend` Min Nothing = x
    mappend (Min a) (Min a') = Min (min a a')

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' x = getMin $ foldMap (Min . Just) x

data Max a = Max { getMax :: Maybe a } deriving (Show, Eq)

instance Ord a => Monoid (Max a) where
    mempty = Max Nothing
    Max Nothing `mappend` x = x
    x `mappend` Max Nothing = x
    mappend (Max a) (Max a') = Max (max a a')

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' x = getMax $ foldMap (Max . Just) x

null' :: (Foldable t) => t a -> Bool
null' = foldr (\_ _ -> False) True

length' :: (Foldable t) => t a -> Int
length' = foldr(\_ acc -> acc+1) 0

toList' :: (Foldable t) => t a -> [a]
toList' = foldr (\x acc -> x : acc) []

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (mappend . f) mempty

spec :: Spec
spec = do
    describe "Foldable Demos" $ do
        it "can fold with Identity" $ do
            foldr (*) 1 (Identity 5) `shouldBe` 5
            foldr (*) 5 (Identity 5) `shouldBe` 25
            let fm = foldMap (*5)
            getProduct(fm (Identity 100) :: Product Integer)
                `shouldBe` 500
        it "can fold with Maybe" $ do
            foldr (+) 1 Nothing `shouldBe` 1
            let fm = foldMap (+1)
            getSum (fm Nothing :: Sum Integer) `shouldBe` 0
            foldr (+) 1 (Just 3) `shouldBe` 4
            getSum ((fm (Just 3)) :: Sum Integer) `shouldBe` 4
        it "can fold with a custom Maybe - Optional" $ do
            getSum ((foldMap (+1) Nada) :: Sum Int)
                `shouldBe` 0
            getProduct ((foldMap (+1) Nada) :: Product Int)
                `shouldBe` 1
    describe "Some basic derived operations" $ do
        it "can convert a items to list" $ do
            toList (Just 1) `shouldBe` [1]
            let xs = [Just 1, Just 2, Just 3]
            map toList xs `shouldBe` [[1],[2],[3]]
            let ys = [Just 1, Just 2, Nothing]
            concatMap toList ys `shouldBe` [1,2]
        it "can tell if a list is empty of not" $ do
            null (Left 3) `shouldBe` True
            null [] `shouldBe` True
            null Nothing `shouldBe` True
            null (1, 2) `shouldBe` False
            let xs = [Just 1, Just 2, Nothing]
            fmap null xs `shouldBe` [False, False, True]
        it "can tell the length of a list" $ do
            length (1, 2) `shouldBe` 1
            let xs = [(1,2),(3,4),(5,6)]
            length xs `shouldBe` 3
            fmap length xs `shouldBe` [1,1,1]
            fmap length Just [1,2,3] `shouldBe` 1
        it "can check for `elem`" $ do
            elem 2 (Just 3) `shouldBe` False
            elem True (Left False) `shouldBe` False
            elem True (Left True) `shouldBe` False
            elem True (Right False) `shouldBe` False
            elem True (Right True) `shouldBe` True
            let xs = [Right 1, Right 2, Right 3]
            fmap (elem 3) xs `shouldBe` [False,False,True]
        it "finds maximum and minimum" $ do
            maximum [10, 12, 33, 5] `shouldBe` 33
            let xs = [Just 2, Just 10, Just 4]
            fmap maximum xs `shouldBe` [2,10,4]
            fmap maximum (Just [3,7,10,2]) `shouldBe` Just 10
            minimum "julie" `shouldBe` 'e'
            fmap minimum (Just "julie") `shouldBe` Just 'e'
            let xs = map Just "jul"
            xs `shouldBe` [Just 'j', Just 'u', Just 'l']
            fmap minimum xs `shouldBe` "jul"
        it "can calculate `sum` and `product`" $ do
            sum (5,7) `shouldBe` 7
            fmap sum [(7,5),(3,4)] `shouldBe` [5,4]
            fmap sum (Just [1,2,3,4,5]) `shouldBe` Just 15
            product Nothing `shouldBe` 1
            fmap product (Just []) `shouldBe` Just 1
            fmap product (Right [1,2,3] :: Either String [Int])
                `shouldBe` Right 6
        it "verifies the custom implementation" $ do
            sum' [1,2,3] `shouldBe` 6
            sum' [] `shouldBe` 0
            fmap sum' (Just [1,2,3,4,5]) `shouldBe` Just 15
            product' Nothing `shouldBe` 1
            product' [1,2,3] `shouldBe` 6
            fmap product' (Just [1,2,3]) `shouldBe` Just 6
            elem' 3 [1..3] `shouldBe` True
            elem' 4 [1..3] `shouldBe` False
            minimum' [2,1,3] `shouldBe` Just 1
            minimum' [] `shouldBe` (Nothing :: Maybe Int)
            fmap minimum' (Just [2,1,4]) `shouldBe` (Just (Just 1))
            maximum' [2,1,3] `shouldBe` Just 3
            maximum' [] `shouldBe` (Nothing :: Maybe Int)
            fmap maximum' (Just [2,1,4]) `shouldBe` (Just (Just 4))
            null' Nothing `shouldBe` True
            null' [] `shouldBe` True
            null' [1] `shouldBe` False
            length' [] `shouldBe` 0
            length' [1] `shouldBe` 1
            length' Nothing `shouldBe` 0

