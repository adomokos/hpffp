module Hpffp.Functors02Spec where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Function

main :: IO ()
main = hspec spec

{-
    Functor laws:
    fmap id = id
    fmap (p . q) = (fmap p) . (fmap q)
-}

data Or a b = First a
            | Second b
            deriving (Eq, Show)

instance Functor (Or a) where
    fmap _ (First a) = First a
    fmap f (Second b) = Second (f b)

functorIdentity :: (Functor f, Eq (f a)) =>
                    f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) =>
                    (a -> b)
                  -> (b -> c)
                  -> f a
                  -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Eq (f c), Functor f) =>
    f a
    -> Fun a b
    -> Fun b c
    -> Bool
functorCompose' x (Fun _ f) (Fun _ g) =
    (fmap (g . f) x) == (fmap g . fmap f $ x)

-- 1.

newtype Identity a = Identity a deriving (Show, Eq)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

genId :: Arbitrary a => Gen (Identity a)
genId = do
    x <- arbitrary
    return $ Identity x

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = genId

-- 2. Pair

data Pair a = Pair a a deriving (Show, Eq)

instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)

genPair :: Arbitrary a => Gen (Pair a)
genPair = do
    x <- arbitrary
    y <- arbitrary
    return $ Pair x y

instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary = genPair

-- 3. Two a b

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

genThree :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Three a b c)
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
    fmap f (Four k l m n) = Four k l m (f n)

genFour :: (Arbitrary a,
            Arbitrary b,
            Arbitrary c,
            Arbitrary d)
            => Gen (Four a b c d)
genFour = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    w <- arbitrary
    return $ Four x y z w

instance (Arbitrary a,
          Arbitrary b,
          Arbitrary c,
          Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = genFour


-- 7.

data Four' a b = Four' a a a b deriving (Show, Eq)

instance Functor (Four' a) where
    fmap f (Four' a b c d) = Four' a b c (f d)

genFour' :: (Arbitrary a,
            Arbitrary b)
            => Gen (Four' a b)
genFour' = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    w <- arbitrary
    return $ Four' x y z w

instance (Arbitrary a,
          Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = genFour'
-- =================

type IntToInt = Fun Int Int
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool
type IdFC = Identity Int -> IntToInt -> IntToInt -> Bool
type PairFC = Pair Int -> IntToInt -> IntToInt -> Bool
type TwoFC = Two Int Int -> IntToInt -> IntToInt -> Bool
type ThreeFC = Three Int Int Int -> IntToInt -> IntToInt -> Bool
type ThreeFC' = Three' Int Int -> IntToInt -> IntToInt -> Bool
type FourFC = Four Int Int Int Int -> IntToInt -> IntToInt -> Bool
type FourFC' = Four' Int Int -> IntToInt -> IntToInt -> Bool

spec :: Spec
spec = do
    describe "More Functors" $ do
        it "can work with complex types" $ do
            let a = Two "hello" 3
            fmap (+2) a `shouldBe` Two "hello" 5
            let b = First "Hello"
            fmap (+2) b `shouldBe` First "Hello"
            let c = Second 3 :: Or String Int
            fmap (+2) c `shouldBe` Second 5
        it "can test identity with QuickCheck" $ property $
            (functorIdentity :: [Int] -> Bool)
        it "can test composability with QuickCheck" $ property $
            ((\x -> functorCompose (+1) (*2) (x :: [Int])))
        it "verifies newtype Identity" $ property $ do
            (functorIdentity :: (Identity Int) -> Bool)
        it "verifies newtype Identity" $ property $ do
            (functorCompose' :: IdFC)
        it "verifies identity for Pair a" $ property $ do
            (functorIdentity :: (Pair Int) -> Bool)
        it "verifies composition for Pair a" $ property $ do
            (functorCompose' :: PairFC)
        it "verifies identity for Two a b" $ property $ do
            (functorIdentity :: (Two Int String) -> Bool)
        it "verifies composition for Two a b" $ property $ do
            (functorCompose' :: TwoFC)
        it "verifies identity for Three a b c" $ property $ do
            (functorIdentity :: (Three Int String Bool) -> Bool)
        it "verifies composition for Three a b c" $ property $ do
            (functorCompose' :: ThreeFC)
        it "verifies identity for Three' a b" $ property $ do
            (functorIdentity :: (Three' Int String) -> Bool)
        it "verifies composition for Three' a b" $ property $ do
            (functorCompose' :: ThreeFC')
        it "verifies identity for Four a b c d" $ property $ do
            (functorIdentity :: (Four Int String Bool Char) -> Bool)
        it "verifies composition for Four a b c d" $ property $ do
            (functorCompose' :: FourFC)
        it "verifies identity for Four' a b" $ property $ do
            (functorIdentity :: (Four' Int String) -> Bool)
        it "verifies composition for Four' a b" $ property $ do
            (functorCompose' :: FourFC')
