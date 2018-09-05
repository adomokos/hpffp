{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

spec :: Spec
spec = do
    describe "Monad transformers" $ do
        it "can work with MaybeT" $ do
            let x = MaybeT $ Just (Just 2)
            runMaybeT (fmap (+1) x) `shouldBe` Just (Just 3)
            let y = MaybeT $ [Just "No",Nothing]
            runMaybeT (fmap (++"!") y)
                `shouldBe` [Just "No!",Nothing]
