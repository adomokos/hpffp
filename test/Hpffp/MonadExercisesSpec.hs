module Hpffp.MonadExercisesSpec where

import Test.Hspec
import Test.Hspec.Checkers
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Monoid

main :: IO ()
main = hspec spec

-- Exerice 1.

data Nope a = NopeDotJpg deriving (Show, Eq)

instance Functor Nope where
    fmap _ _ = NopeDotJpg

instance Applicative Nope where
    pure _ = NopeDotJpg
    _ <*> _ = NopeDotJpg

instance Monad Nope where
    return = pure
    _ >>= _ = NopeDotJpg

instance Arbitrary a => Arbitrary (Nope a) where
    arbitrary = genNope

genNope :: Gen (Nope a)
genNope = return NopeDotJpg

instance Eq a => EqProp (Nope a) where
    (=-=) = eq

-- Exercise 2.

data PEither b a =
    PLeft a
  | PRight b deriving (Show, Eq)

instance Functor (PEither b) where
    fmap f (PLeft a) = PLeft (f a)
    fmap _ (PRight b) = PRight b

instance Applicative (PEither b) where
    pure = PLeft
    PLeft _ <*> PRight x = PRight x
    PLeft f <*> PLeft x = PLeft (f x)
    PRight x <*> _ = PRight x

instance Monad (PEither b) where
    return = pure
    PLeft a >>= f = f a
    PRight b >>= _ = PRight b


instance (Arbitrary a, Arbitrary b) => Arbitrary (PEither b a) where
    arbitrary = genPEither

genPEither :: (Arbitrary b, Arbitrary a) => Gen (PEither b a)
genPEither = do
    b <- arbitrary
    a <- arbitrary
    elements [PRight b, PLeft a]

instance (Eq b, Eq a) => EqProp (PEither b a) where
    (=-=) = eq

-- Exercise 3.

newtype Identity a = Identity a
    deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
    pure = Identity
    Identity f <*> Identity x = Identity (f x)

instance Monad Identity where
    return = pure
    Identity x >>= f = f x

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = genId

genId :: Arbitrary a => Gen (Identity a)
genId = do
    x <- arbitrary
    return $ Identity x

instance Eq a => EqProp (Identity a) where
    (=-=) = eq

-- 4. List
data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons h t) = Cons (f h) (fmap f t)

instance Monoid (List a) where
    mempty = Nil
    mappend Nil lb = lb
    mappend (Cons a la) lb = Cons a (la `mappend` lb)

instance Applicative List where
    pure a = Cons a Nil

    (<*>) Nil _ = Nil
    (<*>) (Cons f fs) as = mappend (fmap f as) (fs <*> as)

instance Monad List where
    return x = Cons x Nil
    Nil >>= _ = Nil
    Cons head tail >>= f = mappend (f head) (tail >>= f)

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = genList

genList :: Arbitrary a => Gen (List a)
genList = do
    h <- arbitrary
    t <- genList
    frequency [(3, return $ Cons h t),
               (1, return Nil)]

instance Eq a => EqProp (List a) where
    (=-=) = eq

-- Just an abbreviation

type I = Int

spec :: Spec
spec = do
    describe "Monad Exercises" $ do
        describe "Nope - exercice 1." $ do
            let subject :: Nope (I,I,I)
                subject = undefined
            testBatch $ functor subject
            testBatch $ applicative subject
            testBatch $ monad subject
        describe "PEither - exercice 2." $ do
            let subject :: PEither I (I,I,I)
                subject = undefined
            testBatch $ functor subject
            testBatch $ applicative subject
            testBatch $ monad subject
        describe "Identity - exercise 3." $ do
            let subject :: Identity (I,I,I)
                subject = undefined
            testBatch $ functor subject
            testBatch $ applicative subject
            testBatch $ monad subject
        describe "List - exercise 4." $ do
            let subject :: List (I,I,I)
                subject = undefined
            testBatch $ functor subject
            testBatch $ applicative subject
            testBatch $ monad subject

