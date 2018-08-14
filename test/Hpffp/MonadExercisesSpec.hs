module Hpffp.MonadExercisesSpec where

import Test.Hspec
import Test.Hspec.Checkers
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Monoid
import Control.Applicative (liftA2)

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

-- Functions

j :: Monad m => m (m a) -> m a
j = (=<<) id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftA2

a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)

-- Needs recursion
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x:xs) f = do
    x' <- f x
    fmap ((:) x') (meh xs f)

flipType :: (Monad m) => [m a] -> m [a]
flipType = (flip meh) id

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
        describe "Functions" $ do
            it "lifts" $ do
                j [[1,2], [], [3]]
                    `shouldBe` [1,2,3]
                j (Just (Just 1)) `shouldBe` Just 1
                j ((Just Nothing) :: Maybe (Maybe I)) `shouldBe` Nothing
                j (Nothing :: Maybe (Maybe I)) `shouldBe` Nothing
                l1 (+1) (Just 2) `shouldBe` Just 3
                l1 (+1) Nothing `shouldBe` Nothing
                l2 (+) (Just 2) (Just 3) `shouldBe` Just 5
                l2 (+) Nothing (Just 3) `shouldBe` Nothing
                a (Just 2) (Just (+3)) `shouldBe` Just 5
                a Nothing (Just (+3)) `shouldBe` Nothing
                a (Just 2) Nothing `shouldBe` (Nothing :: Maybe Int)
                meh [1..3] (\x -> (Just x)) `shouldBe` Just [1..3]
                meh [] (\x -> (Just x)) `shouldBe` (Just [] :: Maybe [Int])
