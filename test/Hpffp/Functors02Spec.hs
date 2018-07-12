module Hpffp.Functors02Spec where

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

{-
    Functor laws:
    fmap id = id
    fmap (p . q) = (fmap p) . (fmap q)
-}

data Two a b = Two a b deriving (Eq, Show)

data Or a b = First a
            | Second b
            deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two a b )= Two a (f b)

instance Functor (Or a) where
    fmap _ (First a) = First a
    fmap f (Second b) = Second (f b)

functorIdentity :: (Functor f, Eq (f a)) =>
                    f a
                    -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) =>
                    (a -> b)
                  -> (b -> c)
                  -> f a
                  -> Bool
functorCompose f g x =
    (fmap g (fmap f x)) == (fmap (g . f) x)

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
