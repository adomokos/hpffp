module Hpffp.TraversableExercisesSpec where

import Test.Hspec
import Test.Hspec.Checkers
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Foldable

main :: IO ()
main = hspec spec

-- Identity

newtype Identity a = Identity a
    deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
    pure x = Identity x
    (Identity x) <*> (Identity y) = Identity (x y)

instance Foldable Identity where
    foldMap f (Identity x) = f x
    foldr f z (Identity x) = f x z

instance Traversable Identity where
    traverse f (Identity x) = Identity <$> f x

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
    (=-=) = eq

-- Constant

newtype Constant a b = Constant { getConstant :: a }
    deriving (Show, Eq)

instance Functor (Constant b)  where
    fmap _ (Constant x) = Constant x

instance Monoid a => Applicative (Constant a) where
    pure _ = Constant { getConstant = mempty }
    x <*> x' = Constant (getConstant x `mappend` getConstant x')

instance Foldable (Constant a) where
    foldMap _ _ = mempty

instance Traversable (Constant a) where
    traverse _ (Constant x) = Constant <$> pure x

-- for testing

instance Arbitrary a => Arbitrary (Constant a b) where
    arbitrary = genConst

genConst :: Arbitrary a => Gen (Constant a b)
genConst = do
    a <- arbitrary
    return $ Constant a

instance (Eq a, Eq b) => EqProp (Constant a b) where
    x =-= x' = getConstant x `eq` getConstant x'

-- Maybe

data Optional a = Nada | Yep a deriving (Show, Eq)

instance Monoid a => Monoid (Optional a) where
    mempty = Nada
    Nada `mappend` _ = Nada
    _ `mappend` Nada = Nada
    Yep x `mappend` Yep x' = Yep (x `mappend` x')

instance Functor Optional where
    fmap _ Nada = Nada
    fmap f (Yep x) = Yep (f x)

instance Applicative Optional where
    pure = Yep
    Nada <*> _ = Nada
    _ <*> Nada = Nada
    (Yep f) <*> (Yep x) = Yep (f x)

instance Foldable Optional where
    foldMap f Nada = mempty
    foldMap f (Yep x) = f x

instance Traversable Optional where
    traverse _ Nada = pure Nada
    traverse f (Yep x) = Yep <$> f x

-- for testing
instance Eq a => EqProp (Optional a) where
    (=-=) = eq

instance Arbitrary a => Arbitrary (Optional a) where
    arbitrary = genYep

genYep :: Arbitrary a => Gen (Optional a)
genYep = do
    x <- arbitrary
    return $ Yep x

-- List
data List a = Nil | Cons a (List a) deriving (Show, Eq)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons h l) = Cons (f h) (fmap f l)

instance Foldable List where
    foldMap _ Nil = mempty
    foldMap f (Cons h t) = f h `mappend` foldMap f t

instance Traversable List where
    traverse _ Nil = pure Nil
    traverse f (Cons h t) = Cons <$> f h <*> traverse f t

-- for testing
instance Eq a => EqProp (List a) where
    (=-=) = eq

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = genList

genList :: Arbitrary a => Gen (List a)
genList = do
    h <- arbitrary
    t <- genList
    frequency [ (3, return $ Cons h t)
              , (1, return Nil)]

-- Big

data Big a b = Big a b b deriving (Show, Eq)

instance Functor (Big a) where
    fmap f (Big x y y') = Big x (f y) (f y')

instance Foldable (Big a) where
    foldMap f (Big _ b b') = f b `mappend` f b'
    foldr f y (Big _ b b') = f b' y

instance Traversable (Big a) where
    traverse f (Big a b b') = (Big a) <$> f b <*> f b'
    sequenceA (Big a b b') = (Big a) <$> b <*> b'

-- for testing
instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
    arbitrary = Big <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Big a b) where
    (=-=) = eq

-- Bigger

data Bigger a b = Bigger a b b b deriving (Show, Eq)

instance Functor (Bigger a) where
    fmap f (Bigger a x y z) = Bigger a (f x) (f y) (f z)

instance Foldable (Bigger a) where
    foldMap f (Bigger _ x y z) = f x `mappend` f y `mappend` f z

instance Traversable (Bigger a) where
    traverse f (Bigger a x y z) = (Bigger a) <$> f x <*> f y <*> f z
    sequenceA (Bigger a x y z) = (Bigger a) <$> x <*> y <*> z

-- for testing
instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
    arbitrary = Bigger <$> arbitrary
                       <*> arbitrary
                       <*> arbitrary
                       <*> arbitrary

instance (Eq a, Eq b) => EqProp (Bigger a b) where
    (=-=) = eq

-- S

data S n a = S (n a) a deriving (Show, Eq)

instance Functor n => Functor (S n) where
    fmap f (S n a) = S (fmap f n) (f a)

instance Foldable n => Foldable (S n) where
    foldMap f (S n a) = foldMap f n `mappend` f a

instance Traversable n => Traversable (S n) where
    traverse f (S n a) = S <$> traverse f n <*> f a

-- for testing

instance (Eq (n a), Eq a) => EqProp (S n a) where
    (=-=) = eq

instance (Arbitrary (n a), CoArbitrary (n a),
          Arbitrary a, CoArbitrary a) =>
    Arbitrary (S n a) where
        arbitrary = genS

genS :: (Arbitrary (n a), CoArbitrary (n a),
         Arbitrary a, CoArbitrary a) =>
        Gen (S n a)
genS = do
    n <- arbitrary
    a <- arbitrary
    return $ S (n a) a

-- Tree

data Tree a = Empty
            | Leaf a
            | Node (Tree a) a (Tree a)
            deriving (Eq, Show)

instance Functor Tree where
    fmap _ Empty = Empty
    fmap f (Leaf x) = Leaf (f x)
    fmap f (Node n x n') = Node (fmap f n) (f x) (fmap f n')

instance Foldable Tree where
    foldMap _ Empty = mempty
    foldMap f (Leaf x) = f x
    foldMap f (Node n x n') =
        foldMap f n `mappend` f x `mappend` foldMap f n'

    foldr _ y Empty = y
    foldr f y (Leaf x) = f x y
    foldr f y (Node l x r) = f x $ foldr f (foldr f y r) l

instance Traversable Tree where
    traverse _ Empty = pure Empty
    traverse f (Leaf x) = Leaf <$> f x
    traverse f (Node n x n') =
        Node <$> traverse f n <*> f x <*> traverse f n'

-- for testing

instance Eq a => EqProp (Tree a) where
    (=-=) = eq

instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = genTree

genTree :: Arbitrary a => Gen (Tree a)
genTree = do
    x <- arbitrary
    n <- genTree
    n' <- genTree
    frequency [ (1, return Empty)
              , (2, return $ Leaf x)
              , (2, return $ Node n x n') ]

spec :: Spec
spec = do
    describe "Traversable Exercises" $ do
        describe "works for Identity" $ do
            let subject :: Identity (Int,Int,[Int])
                subject = undefined
            testBatch $ functor subject
            testBatch $ applicative subject
            testBatch $ traversable subject
        describe "works for Constant" $ do
            let subject :: Constant [Int] (Int,Int,[Int])
                subject = undefined
            testBatch $ functor subject
            testBatch $ applicative subject
            testBatch $ traversable subject
        describe "works for Optional" $ do
            let subject :: Optional (Int,Int,[Int])
                subject = undefined
            testBatch $ functor subject
            testBatch $ applicative subject
            testBatch $ traversable subject
        describe "works for List" $ do
            let subject :: List (Int,Int,[Int])
                subject = undefined
            testBatch $ functor subject
            testBatch $ traversable subject
        describe "works for Big" $ do
            let subject :: Big Int (Int,Int,[Int])
                subject = undefined
            testBatch $ functor subject
            testBatch $ traversable subject
        describe "works for Bigger" $ do
            let subject :: Bigger Int (Int,Int,[Int])
                subject = undefined
            testBatch $ functor subject
            testBatch $ traversable subject
        describe "works for S" $ do
            let subject :: S Maybe (Int, Int, [Int])
                subject = undefined
            testBatch $ functor subject
            testBatch $ traversable subject
        describe "works for Tree" $ do
            let subject :: Tree (Int, Int, [Int])
                subject = undefined
            testBatch $ functor subject
            testBatch $ traversable subject
