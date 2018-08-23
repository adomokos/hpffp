{-# LANGUAGE InstanceSigs #-}

module Hpffp.MonadOfFunctionsSpec where

import Test.Hspec
import Control.Monad (join)

main :: IO ()
main = hspec spec

foo :: (Functor f, Num a) => f a -> f a
foo r = fmap (+1) r

bar :: Foldable f => t -> f a -> (t, Int)
bar r t = (r, length t)

froot :: Num a => [a] -> ([a], Int)
froot r = (map (+1) r, length r)

barOne :: Foldable t => t a -> (t a, Int)
barOne r = (r, length r)

barPlus :: (Functor t, Foldable t, Num a) => t a -> (t a, Int)
barPlus r = (foo r, length r)

frooty :: Num a => [a] -> ([a], Int)
frooty r = bar (foo r) r

frooty' :: Num a => [a] -> ([a], Int)
frooty' = \r -> bar (foo r) r

fooBind :: (r -> a) -> (a -> r -> b) -> (r -> b)
fooBind m k = \r -> k (m r) r

{-
(>>=) :: Monad m =>
     m     a -> (a -> (m    b)) ->  m    b
    (r -> a) -> (a -> (r -> b)) -> (r -> b)

return :: Monad m => a ->      m a
return ::            a -> (->) r a
return ::            a ->   r -> a

-- Look at it side by side with the Applicative:
(<*>) :: (r -> a -> b) -> (r -> a) -> (r -> b)
(>>=) :: (r -> a) -> (a -> r -> b) -> (r -> b)

-- or with the flipped bind
(<*>) :: (r -> a -> b) -> (r -> a) -> (r -> b)
(=<<) :: (a -> r -> b) -> (r -> a) -> (r -> b)
-}

newtype Reader r a =
    Reader { runReader :: r -> a }

instance Functor (Reader a) where
  fmap f (Reader x) =
    Reader $ f . x

instance Applicative (Reader r) where
  pure a = Reader (\r -> a)
  Reader f <*> Reader g =
      Reader (\r -> f r (g r))

instance Monad (Reader r) where
    return = pure

    (>>=) :: Reader r a
          -> (a -> Reader r b)
          -> Reader r b
    (Reader ra) >>= aRb =
        join $ Reader $ \r -> aRb (ra r)

spec :: Spec
spec = do
    describe "Monad of Functions" $ do
        it "works with functions" $ do
            (foo (*3)) 2 `shouldBe` 7
            (bar [1] "Yo") `shouldBe` ([1], 2)
            (froot [1..3]) `shouldBe` ([2..4], 3)
            (barOne [1..3]) `shouldBe` ([1..3], 3)
            (frooty [1..3]) `shouldBe` ([2..4], 3)
            (frooty' [1..3]) `shouldBe` ([2..4], 3)
