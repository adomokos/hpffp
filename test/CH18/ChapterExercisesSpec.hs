module CH18.ChapterExercisesSpec where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Monad
import Control.Applicative

import Prelude hiding (Left, Right)

main :: IO ()
main = hspec spec

-- 1.
data Nope a =
  NopeDotJpg deriving (Show, Eq)

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  NopeDotJpg <*> NopeDotJpg = NopeDotJpg

instance Monad Nope where
  return = pure
  NopeDotJpg >>= _ = NopeDotJpg

instance Arbitrary a => Arbitrary (Nope a) where
  arbitrary = pure NopeDotJpg

instance Eq a => EqProp (Nope a) where
  (=-=) = eq

-- 2.
data PhhhbbtttEither b a =
    Left a
  | Right b
  deriving (Show, Eq)

instance Functor (PhhhbbtttEither b) where
  fmap f (Left x) = Left (f x)
  fmap _ (Right y) = Right y

instance Applicative (PhhhbbtttEither b) where
  pure x = Left x
  Left f <*> Left x = Left (f x)
  Right y <*> _ = Right y
  _ <*> Right y = Right y

instance Monad (PhhhbbtttEither b) where
  return = pure
  Left x >>= f = f x
  Right y >>= _ = Right y

instance (Arbitrary a, Arbitrary b)
  => Arbitrary (PhhhbbtttEither b a) where
  arbitrary = genPhhhbbtttEither

genPhhhbbtttEither :: (Arbitrary b,Arbitrary a)
                   => Gen (PhhhbbtttEither b a)
genPhhhbbtttEither = do
  x <- arbitrary
  y <- arbitrary
  frequency [(2, return $ Left x),
             (2, return $ Right y)]

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither b a) where
  (=-=) = eq

-- 3.

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure x = Identity x
  Identity f <*> Identity x = Identity (f x)

instance Monad Identity where
  return = pure
  Identity x >>= f = f x

genId :: Arbitrary a => Gen (Identity a)
genId = do
    x <- arbitrary
    return $ Identity x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = genId

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

-- 4.

data List a = Nil
            | Cons a (List a)
            deriving (Show, Eq)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x l) = Cons (f x) (fmap f l)

instance Monoid (List a) where
  mempty = Nil
  mappend Nil _ = Nil
  mappend (Cons h t) ls = Cons h (t `mappend` ls)

instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (<*>) (Cons f fs) as = mappend (fmap f as) (fs <*> as)

instance Monad List where
  return = pure
  Nil >>= _ = Nil
  Cons h t >>= f = mappend (f h) (t >>= f)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = genList

genList :: Arbitrary a => Gen (List a)
genList = do
  h <- arbitrary
  t <- genList
  frequency [(3, return $ Cons h t),
             (1, return Nil)]

instance Eq a => EqProp (List a) where (=-=) = eq

-- === Write Functions
-- 1.

j :: Monad m => m (m a) -> m a
j = (=<<) id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l2 :: (Applicative m, Monad m) => (a -> b -> c) -> m a -> m b -> m c
l2 = liftA2

a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)

meh :: Monad m
    => [a] -> (a -> m b) -> m [b]
meh [] _ = pure []
meh (x:xs) f = do
  x' <- f x
  fmap ((:) x') (meh xs f)

flipType :: (Monad m) => [m a] -> m [a]
flipType = flip meh $ id

spec :: Spec
spec = do
  describe "Chapter Exercises" $ do
    it "1. Nope a" $ do
      pending
      quickBatch
        (applicative (undefined :: Nope (String, String, String)))
      quickBatch
        (monad (undefined :: Nope (String, String, String)))
    it "2. PhhhbbtttEither" $ do
      pending
      quickBatch
        (applicative (undefined :: PhhhbbtttEither (String, String, String)
                                                   (Int, Int, Int)))
      quickBatch
        (monad (undefined :: PhhhbbtttEither (String, String, String)
                                             (Int, Int, Int)))
    it "3. Identity" $ do
      pending
      quickBatch
        (applicative (undefined :: Identity (String, String, String)))
      quickBatch
        (monad (undefined :: Identity (String, String, String)))

    it "4. List" $ do
      pending
      quickBatch
        (applicative (undefined :: List (String, String, String)))
      quickBatch
        (monad (undefined :: List (String, String, String)))

  describe "Function creation" $ do
    it "works for function j" $ do
      j [[1, 2], [], [3]] `shouldBe` ([1,2,3] :: [Int])
      j (Just (Just 1)) `shouldBe` (Just 1 :: Maybe Int)
      j (Just (Nothing)) `shouldBe` (Nothing :: Maybe Int)
      j Nothing `shouldBe` (Nothing :: Maybe Int)
