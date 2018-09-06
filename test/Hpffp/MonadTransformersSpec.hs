{-# LANGUAGE InstanceSigs, GeneralizedNewtypeDeriving #-}
module Hpffp.MonadTransformersSpec where

import Test.Hspec

main :: IO ()
main = hspec spec

newtype MaybeT m a =
    MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor m)
        => Functor (MaybeT m) where
    fmap f (MaybeT ma) =
        MaybeT $ (fmap . fmap) f ma

instance (Applicative m)
        => Applicative (MaybeT m) where
    pure x = MaybeT (pure (pure x))
    (MaybeT fab) <*> (MaybeT mma) =
        MaybeT $ (<*>) <$> fab <*> mma

instance (Monad m)
        => Monad (MaybeT m) where
    return = pure
    (>>=) :: MaybeT m a
          -> (a -> MaybeT m b)
          -> MaybeT m b
    (MaybeT ma) >>= f =
        MaybeT $ do
            -- ma :: m (Maybe a)
            -- v :: Maybe a
            v <- ma
            case v of
                Nothing -> return Nothing
                Just y -> runMaybeT (f y)

spec :: Spec
spec = do
    describe "Monad transformers" $ do
        it "can work with MaybeT Functors" $ do
            let x = MaybeT $ Just (Just 2)
            runMaybeT (fmap (+1) x) `shouldBe` Just (Just 3)
            let y = MaybeT $ [Just "No",Nothing]
            runMaybeT (fmap (++"!") y)
                `shouldBe` [Just "No!",Nothing]
        it "can work with MaybeT Monads" $ do
            let x = MaybeT $ [(Just 2), Nothing]
            -- (map . fmap) (*2) [Just 2, Nothing]
            pending


