module Hpffp.ComposeableFunctorSpec where

import Test.Hspec

main :: IO ()
main = hspec spec

newtype Identity a = Identity { runIdentity :: a }

-- We can construct a datatype that corresponds
-- to function `composition`

newtype Compose f g a =
    Compose { getCompose :: f (g a) }
    deriving (Show, Eq)

-- Compare this:
-- Compose :: (* -> *) -> (* -> *) -> * -> *
-- with:
-- (.) :: (b -> c) -> (a -> b) -> a -> c

-- Lifting over Identity
instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance (Functor f, Functor g) =>
        Functor (Compose f g) where
    fmap f (Compose fga) =
        Compose $ (fmap . fmap) f fga

-- This can be generalized to different amounts
-- of structure
newtype One f a =
    One (f a)
    deriving (Eq, Show)

instance Functor f =>
         Functor (One f) where
    fmap f (One fa) = One $ fmap f fa

-- Or one more layer than Compose
newtype Three f g h a =
    Three (f (g (h a)))
    deriving (Show, Eq)

instance (Functor f, Functor g, Functor h)
        => Functor (Three f g h) where
    fmap f (Three fgha) =
        Three $ (fmap . fmap . fmap) f fgha

spec :: Spec
spec = do
    describe "Composing Types" $ do
        it "works for Identity and Compose" $ do
            let result = Compose [Just 1, Nothing]
            getCompose result `shouldBe` [Just 1, Nothing]
        it "can fmap over Compose" $ do
            let xs = [Just 1, Nothing]
            fmap (+1) (Compose xs) `shouldBe`
                Compose [Just 2, Nothing]
