{-# LANGUAGE InstanceSigs, GeneralizedNewtypeDeriving #-}
module Hpffp.MonadTransformersSpec where

import Test.Hspec
import Control.Monad.Trans.Class

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

-- Exercise EitherT
newtype EitherT e m a =
    EitherT { runEitherT :: m (Either e a) }

instance Functor m
        => Functor (EitherT e m) where
    fmap f (EitherT mea) =
        EitherT $ (fmap . fmap) f mea

instance Applicative m
        => Applicative (EitherT e m) where
    pure x = EitherT (pure (pure x))
    EitherT fab <*> EitherT mma =
        EitherT $ (<*>) <$> fab <*> mma

instance Monad m => Monad (EitherT e m) where
    return = pure

    (>>=) :: EitherT e m a
          -> (a -> EitherT e m b)
          -> (EitherT e m b)
    EitherT ma >>= f =
        EitherT $ do
        v <- ma
        case v of 
          Left e -> return $ Left e
          Right a -> runEitherT (f a)

swapEither :: Either e a -> Either a e
swapEither x =
    case x of
      Left e -> Right e
      Right a -> Left a

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT x) = EitherT $ swapEither <$> x

eitherT :: Monad m =>
           (a -> m c)
        -> (b -> m c)
        -> EitherT a m b
        -> m c
eitherT fa fb (EitherT x) = x >>= either fa fb

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
            let f = (\x -> return (x*2))
            runMaybeT(x >>= f) `shouldBe`
                [Just 4, Nothing]
        it "can work with EitherT Functors" $ do
            let x = EitherT $ [Right 2, Left "OK"]
            runEitherT (fmap (+1) x)
                `shouldBe` [(Right 3:: Either String Int), Left "OK"]
        it "can work with EitherT Monad" $ do
            let x = EitherT $ [Right 2, Left "OK"]
            let f = (\x -> return (x*2))
            runEitherT(x >>= f) `shouldBe`
                [Right 4, Left "OK"]
        it "can swap Either" $ do
            let x = Right 2 :: (Either Int Int)
            swapEither x `shouldBe` Left 2
            let y = EitherT $ [Left 2, Right 4]
            runEitherT(swapEitherT y) `shouldBe`
                [Right 2, Left 4]

