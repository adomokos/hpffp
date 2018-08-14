module Hpffp.MonadCompositionSpec where

import Test.Hspec
import Control.Monad

main :: IO ()
main = hspec spec

mcomp :: Monad m =>
        (b -> m c)
     -> (a -> m b)
     -> (a -> m c)
mcomp f g a = join (f <$> (g a))


mcomp'' :: Monad m =>
        (b -> m c)
     -> (a -> m b)
     -> (a -> m c)
mcomp'' f g a = g a >>= f

{-
    (.) :: (b -> c) -> (a -> b) -> a -> c
    (>>=) :: Monad m
       => m a -> (a -> m b) -> m b

    To get Kleisli composition off the ground we have to flip
    some arguments around to make the types work:

    (>=>) :: Monad m
          => (a -> m b) -> (b -> m c) -> a -> m c
    flip (.)
          :: (a -> b)   -> (b -> c)   -> a -> c

-}


spec :: Spec
spec = do
    describe "Monad Application and Composition" $ do
        it "works with functors" $ do
            fmap ((+1) . (+2)) [1..5] `shouldBe` [4..8]
            (fmap (+1) . fmap (+2)) [1..5]
                `shouldBe` [4..8]
        it "works as 'Kleisli' composition" $ do
            pending


