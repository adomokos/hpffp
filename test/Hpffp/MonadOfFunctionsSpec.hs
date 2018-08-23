module Hpffp.MonadOfFunctionsSpec where

import Test.Hspec

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
-}

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
