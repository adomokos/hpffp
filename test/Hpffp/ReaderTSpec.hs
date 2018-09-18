{-# LANGUAGE InstanceSigs #-}
module Hpffp.ReaderTSpec where

import Test.Hspec

main :: IO ()
main = hspec spec

newtype ReaderT r m a =
    ReaderT { runReaderT :: r -> m a }

instance (Functor m)
        => Functor (ReaderT r m) where
    fmap f (ReaderT rma) =
        ReaderT $ (fmap . fmap) f rma

instance (Applicative m)
        => Applicative (ReaderT r m) where
    pure a = ReaderT (pure (pure a))
    (ReaderT fmab) <*> (ReaderT rma) =
        ReaderT $ (<*>) <$> fmab <*> rma

instance (Monad m)
        => Monad (ReaderT r m)  where
    return = pure

    (>>=) :: ReaderT r m a
          -> (a -> ReaderT r m b)
          -> ReaderT r m b
    (ReaderT rma) >>= f =
        ReaderT $ \r -> do
            a <- rma r
            runReaderT (f a) r

-- newtype Reader r a =
    -- Reader { runReader :: r -> a }

newtype Identity a = Identity { runIdentity :: a }

spec :: Spec
spec = do
    describe "ReaderT Intro" $ do
        it "can work with monadic Functors" $ do
            -- let x = MaybeT (Identity (Just 1))
            -- runMaybeT ((+1) <$> x) `shouldBe` 
                -- Identity { runIdentity = Just 2 }
            pending
