{-# LANGUAGE InstanceSigs #-}
module Hpffp.ComposeableApplicativeSpec where

import Test.Hspec

main :: IO ()
main = hspec spec

newtype Compose f g a =
    Compose { getCompose :: f (g a) }
    deriving (Show, Eq)

instance (Functor f, Functor g) =>
        Functor (Compose f g) where
    fmap f (Compose fga) =
        Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g)
        => Applicative (Compose f g) where
    pure :: a -> Compose f g a
    pure a = Compose $ (pure . pure) a

    (<*>) :: Compose f g (a -> b)
          -> Compose f g a
          -> Compose f g b
    (Compose f) <*> (Compose a) =
        Compose $ ((<*>) <$> f) <*> a

instance (Foldable f, Foldable g)
        => Foldable (Compose f g) where
    foldMap f (Compose fga) =
        (foldMap . foldMap) f fga

instance (Traversable f, Traversable g)
        => Traversable (Compose f g) where
    traverse :: Applicative f1 => (a -> f1 b)
             -> Compose f g a
             -> f1 (Compose f g b)
    traverse f (Compose fga) =
        Compose <$> (traverse . traverse) f fga

class Bifunctor p where
    {-# MINIMAL bimap | first, second #-}

    bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
    bimap f g = first f . second g

    first :: (a -> b) -> p a c -> p b c
    first f = bimap f id

    second :: (b -> c) -> p a b -> p a c
    second = bimap id

data Deux a b = Deux a b deriving (Show, Eq)

instance Bifunctor Deux where
    bimap f g = first f . second g
    first f (Deux a b) = Deux (f a) b
    second f (Deux a b) = Deux a (f b)

newtype Const a b = Const a deriving (Show, Eq)

instance Bifunctor Const where
    bimap f g = first f . second g
    first f (Const a) = Const (f a)
    second _ (Const a) = Const a

data Drei a b c = Drei a b c deriving (Show, Eq)

instance Bifunctor (Drei a) where
    bimap f g = first f . second g
    first f (Drei a b c) = Drei a (f b) c
    second f (Drei a b c) = Drei a b (f c)

data SuperDrei a b c = SuperDrei a b
    deriving (Show, Eq)

instance Bifunctor (SuperDrei a) where
    bimap f g = first f . second g
    first f (SuperDrei a b) = SuperDrei a (f b)
    second _ (SuperDrei a b) = SuperDrei a b

newtype SemiDrei a b c = SemiDrei a
    deriving (Show, Eq)

instance Bifunctor (SemiDrei a) where
    bimap f g = first f . second g
    first _ (SemiDrei a) = SemiDrei a
    second _ (SemiDrei a) = SemiDrei a

data Quadriceps a b c d = Quadriceps a b c d
    deriving (Show, Eq)

instance Bifunctor (Quadriceps a b) where
    bimap f g = first f . second g
    first f (Quadriceps a b c d) = Quadriceps a b (f c) d
    second g (Quadriceps a b c d) = Quadriceps a b c (g d)

instance Bifunctor Either where
    bimap f g = first f . second g
    first f (Left x) = Left (f x)
    first _ (Right y) = Right y
    second _ (Left x) = Left x
    second g (Right y) = Right (g y)

spec :: Spec
spec = do
    describe "Composing Applicatives" $
        it "works with Compose" $ do
            let a = Compose [Just 1, Nothing]
            let b = Compose [Just 2, Nothing]
            (+) <$> a <*> b `shouldBe`
                Compose [Just 3, Nothing, Nothing, Nothing]
    describe "Bifunctor" $ do
        it "works with Deux" $ do
            let d = Deux 3 "OK"
            bimap (+2) (++"!") d `shouldBe` Deux 5 "OK!"
            first (+2) d `shouldBe` Deux 5 "OK"
            second (++"!") d `shouldBe` Deux 3 "OK!"
        it "works with Const" $ do
            let c = Const 3
            bimap (+2) (++"!") c `shouldBe` Const 5
            first (+2) c `shouldBe` Const 5
            second (++"!") c `shouldBe` Const 3
        it "works with Drei" $ do
            let d = Drei 'a' 2 "OK"
            bimap (+2) (++"!") d `shouldBe` Drei 'a' 4 "OK!"
            first (+2) d `shouldBe` Drei 'a' 4 "OK"
            second (++"!") d `shouldBe` Drei 'a' 2 "OK!"
        it "works with SuperDrei" $ do
            let sd = SuperDrei 'a' 2
            bimap (+2) (++"!") sd `shouldBe` SuperDrei 'a' 4
            first (+2) sd `shouldBe` SuperDrei 'a' 4
            second (+2) sd `shouldBe` sd
        it "works with SemiDrei" $ do
            let smd = SemiDrei 2
            bimap (+2) (++"!") smd `shouldBe` smd
            first (+2) smd `shouldBe` smd
            first (++"!") smd `shouldBe` smd
        it "works with Quadriceps" $ do
            let qs = Quadriceps 'c' False 2 "OK"
            bimap (+2) (++"!") qs
                `shouldBe` Quadriceps 'c' False 4 "OK!"
            first (+2) qs
                `shouldBe` Quadriceps 'c' False 4 "OK"
            second (++"!") qs
                `shouldBe` Quadriceps 'c' False 2 "OK!"
        it "works with Either" $ do
            let x = Left 2
                y = Right "OK"
            bimap (+2) (++"!") x `shouldBe` Left 4
            bimap (+2) (++"!") y `shouldBe` Right "OK!"
            first (+2) x `shouldBe` (Left 4 :: Either Int String)
            first (+2) y `shouldBe` (Right "OK" :: Either Int String)
            second (++"!") x `shouldBe` (Left 2 :: Either Int String)
            second (++"!") y `shouldBe` (Right "OK!" :: Either Int String)
