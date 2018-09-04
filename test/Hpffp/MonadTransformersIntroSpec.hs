{-# LANGUAGE InstanceSigs #-}
module Hpffp.MonadTransformersIntroSpec where

import Test.Hspec
import Control.Monad (join)

main :: IO ()
main = hspec spec

{-
   You can put two monads together, but you don't get a new Monad
   instance.
   The monad tranformer is a type constructor that takes a Monad
   as an argument and returns a Monad as a result.
-}

-- Plain old Identity. 'a' can be
-- something with more structure,
-- but it's not required and Identity
-- won't know anything about it.
newtype Identity a =
    Identity { runIdentity :: a }
    deriving (Eq, Show)

-- The idenity monad transformer, serving
-- only to specify that additional
-- structure should exist.
newtype IdentityT f a =
    IdentityT { runIdentityT :: f a }
    deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

-- The fa argument is the value inside the IdentityT with
-- the (untouchable) structure wrapped around it.
instance (Functor m)
        => Functor (IdentityT m) where
    fmap f (IdentityT fa) =
        IdentityT (fmap f fa)

instance Applicative Identity where
    pure = Identity

    (Identity f) <*> (Identity a) =
        Identity (f a)

instance (Applicative m)
        => Applicative (IdentityT m) where
    pure x = IdentityT (pure x)

    (IdentityT fab) <*> (IdentityT fa) =
        IdentityT (fab <*> fa)

instance Monad Identity where
    return = pure
    (Identity a) >>= f = f a

instance (Monad m)
        => Monad (IdentityT m) where
    return = pure
    (>>=) :: IdentityT m a
          -> (a -> IdentityT m b)
          -> IdentityT m b
    (IdentityT ma) >>= f =
        IdentityT $ ma >>= runIdentityT . f

{-
   The basic pattern that many monad transformers are enabling
   us to cope with is the following type transitions,
   where m is the polymorphic, outer structure and T is some
   concrete type the transformer is for. For example, in the 
   above, T would be IdentityT:
   m (T m b) -> m (m b) -> m b -> T m b
-}

spec :: Spec
spec = do
    describe "Monad transformers" $ do
        it "can do Monadic stacking" $ do
            fmap (+1) (Just 1) `shouldBe` Just 2
            let result = Just (1, "lol", [1,2])
            (,,) <$> Just 1 <*> Just "lol" <*> Just [1,2]
                `shouldBe` result
        it "can run a Monad Transformer" $ do
            let sumR = return . (+1)
                result = IdentityT [1,2,3] >>= sumR
            result `shouldBe` IdentityT [2,3,4]
            let add1 = return . (+1)
                result = IdentityT (Just 3) >>= add1
            result `shouldBe` IdentityT (Just 4)
