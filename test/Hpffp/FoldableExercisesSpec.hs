module Hpffp.FoldableExercisesSpec where

import Test.Hspec
import Data.Foldable
import Data.Monoid

main :: IO ()
main = hspec spec

data Constant a b = Constant b deriving (Show, Eq)

instance Foldable (Constant a) where
    foldMap f (Constant x) = mempty

data Two a b = Two a b deriving (Show, Eq)

instance Foldable (Two a) where
    foldMap f (Two a b) = f b

data Three a b c = Three a b c

instance Foldable (Three a b) where
    foldMap f (Three a b c) = f c

data Three' a b = Three' a b b

instance Foldable (Three' a) where
    foldMap f (Three' a b b') = mappend (f b) (f b')

data Four' a b = Four' a b b b

instance Foldable (Four' a) where
    foldMap f (Four' a b b' b'') = (f b) `mappend` (f b') `mappend` (f b'')

filterF :: (Applicative f, Foldable f, Monoid (f a)) =>
            (a -> Bool) -> f a -> f a
filterF f = foldMap (\a -> if f a then pure a else mempty)

spec :: Spec
spec = do
    describe "Foldable Exercises" $ do
        it "works with custom types" $ do
            foldMap (++"!!") (Constant "abc")
                `shouldBe` ""
            foldMap (++[2]) (Two "Yo" [1,2,3])
                `shouldBe` [1,2,3,2]
            foldMap (++[4]) (Three "Yo" "Hey" [1,2,3])
                `shouldBe` [1,2,3,4]
            foldMap (++[3]) (Three' "Yo" [1,2] [3,4])
                `shouldBe` [1,2,3,3,4,3]
            foldMap (++[10]) (Four' "Yo" [1,2] [3,4] [5,6])
                `shouldBe` [1,2,10,3,4,10,5,6,10]
            filterF (\x -> x > 2) [1,2,3] `shouldBe` [3]
