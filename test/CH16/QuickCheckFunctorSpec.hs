module CH16.QuickCheckFunctorSpec where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

main :: IO ()
main = hspec spec

-- fmap id = id
-- fmap (p . q) = (fmap p) . (fmap q)

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) =>
                      (a -> b)
                   -> (b -> c)
                   -> f a
                   -> Bool
functorCompose f g x =
  (fmap g (fmap f x)) == (fmap (g . f) x)

prop_f :: [Int] -> Bool
prop_f x = functorIdentity x

prop_fc :: [Int] -> Bool
prop_fc x = functorCompose (+1) (*2) x

-- Exercises

-- 1.
newtype Identity a = Identity a deriving (Show, Eq)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

genId :: Arbitrary a => Gen (Identity a)
genId = do
    x <- arbitrary
    return $ Identity x

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = genId

-- 2.
data Pair a = Pair a a deriving (Show, Eq)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

genPair :: Arbitrary a => Gen (Pair a)
genPair = do
  x <- arbitrary
  y <- arbitrary
  return $ Pair x y

instance Arbitrary a  => Arbitrary (Pair a) where
  arbitrary = genPair

-- 3.
data Two a b = Two a b deriving (Show, Eq)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

genTwo :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
genTwo = do
  x <- arbitrary
  y <- arbitrary
  return $ Two x y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = genTwo

-- 4.
data Three a b c = Three a b c deriving (Show, Eq)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

genThree :: (Arbitrary a, Arbitrary b, Arbitrary c)
         => Gen (Three a b c)
genThree = do
  x <- arbitrary
  y <- arbitrary
  z <- arbitrary
  return $ Three x y z

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = genThree

-- 5.

data Three' a b = Three' a b b deriving (Show, Eq)

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

genThree' :: (Arbitrary a, Arbitrary b) => Gen (Three' a b)
genThree' = do
  x <- arbitrary
  y <- arbitrary
  z <- arbitrary
  return $ Three' x y z

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = genThree'

-- 6.
data Four a b c d = Four a b c d deriving (Show, Eq)

instance Functor (Four a b c) where
  fmap f (Four j k l m) = Four j k l (f m)

genFour :: (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
        => Gen (Four a b c d)
genFour = do
  j <- arbitrary
  k <- arbitrary
  l <- arbitrary
  m <- arbitrary
  return $ Four j k l m

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
  => Arbitrary (Four a b c d) where
    arbitrary = genFour

-- 7.
data Four' a b = Four' a a a b deriving (Show, Eq)

instance Functor (Four' a) where
  fmap f (Four' j k l m) = Four' j k l (f m)

genFour' :: (Arbitrary a, Arbitrary b) => Gen (Four' a b)
genFour' = do
  j <- arbitrary
  k <- arbitrary
  l <- arbitrary
  m <- arbitrary
  return $ Four' j k l m

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = genFour'

spec :: Spec
spec = do
  describe "QuickCheck Functor" $ do
    prop "verifies functor Identity" $ prop_f
    prop "verifies functor Composition" $ prop_fc
  describe "Exercises" $ do
    describe "Identity" $ do
      prop "verifies Identity" $ (functorIdentity :: (Identity Int) -> Bool)
      prop "verifies Compose" $ (functorCompose (*3) (+2) :: (Identity Int) -> Bool)
    describe "Pair" $ do
      prop "verifies Identity" $ (functorIdentity :: (Pair Int) -> Bool)
      prop "verifies Compose" $ (functorCompose (*3) (+2) :: (Pair Int) -> Bool)
    describe "Two" $ do
      prop "verifies Identity" $ (functorIdentity :: (Two String Int) -> Bool)
      prop "verifies Compose" $ (functorCompose (*3) (+2) :: (Two String Int) -> Bool)
    describe "Three" $ do
      prop "verifies Identity" $ (functorIdentity :: (Three (Maybe String) String Int) -> Bool)
      prop "verifies Compose" $ (functorCompose (*3) (+2) :: (Three (Maybe String) String Int) -> Bool)
    describe "Three'" $ do
      prop "verifies Identity" $ (functorIdentity :: (Three' String Int) -> Bool)
      prop "verifies Compose" $ (functorCompose (*3) (+2) :: (Three' String Int) -> Bool)
    describe "Four" $ do
      prop "verifies Identity" $ (functorIdentity :: (Four Char Bool String Int) -> Bool)
      prop "verifies Compose" $ (functorCompose (*3) (+2) :: (Four Char Bool String Int) -> Bool)
    describe "Four'" $ do
      prop "verifies Identity" $ (functorIdentity :: (Four' String Int) -> Bool)
      prop "verifies Compose" $ (functorCompose (*3) (+2) :: (Four' String Int) -> Bool)

