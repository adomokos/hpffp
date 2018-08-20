module Hpffp.TraversableSpec where

import Test.Hspec
import Data.Foldable
import Data.Monoid
import Data.Maybe
import Data.Functor.Identity
import Data.Functor.Constant

main :: IO ()
main = hspec spec

{-
    You might notice the similarity between fmap and bind
    fmap     :: (a -> b) -> f a -> f b
    (=<<)    :: (a -> m b) -> m a -> m b
    traverse :: (a -> f b) -> t a -> f (t b)
    ...
    traverse f = sequenceA . fmap f

    mapM is traverse

    mapM :: Monad m => (a -> m b) -> [a] -> m [b]
    - contrast with -
    traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

    What traversable is used for?
    Anytime you need to flip two type constructors around, or map something
    and then flip them around, that's probably `Traversable`.
-}

spec :: Spec
spec = do
    describe "Traversable" $ do
        it "traverse various data structures" $ do
            -- compare
            sum [1,2,3] `shouldBe` 6
            fmap sum [Just 1, Just 2, Just 3]
                `shouldBe` [1,2,3]
            fmap product [Just 1, Just 2, Nothing]
                `shouldBe` [1,2,1]
            -- to these
            fmap Just [1,2,3] `shouldBe` [Just 1, Just 2, Just 3]
            sequenceA (fmap Just [1,2,3])
                `shouldBe` Just [1,2,3]
            sequenceA [Just 1, Just 2, Just 3]
                `shouldBe` Just [1,2,3]
            sequenceA [Just 1, Just 2, Nothing]
                `shouldBe` Nothing
            fmap sum (sequenceA [Just 1, Just 2, Just 3])
                `shouldBe` Just 6
            let xs = [Just 3, Just 4, Nothing]
            fmap product (sequenceA xs) `shouldBe` Nothing
        it "handles Maybe values differently with catMaybes" $ do
            catMaybes [Just 1, Just 2, Just 3]
                `shouldBe` [1,2,3]
            catMaybes [Just 1, Just 2, Nothing]
                `shouldBe` [1,2]
            let xs = [Just 1, Just 2, Just 3, Nothing]
            sum (catMaybes xs) `shouldBe` 6
            fmap sum (sequenceA xs) `shouldBe` Nothing
        it "is a combination of sequenceA and fmap" $ do
            fmap Just [1,2,3] `shouldBe` [Just 1, Just 2, Just 3]
            sequenceA (fmap Just [1,2,3])
                `shouldBe` Just [1,2,3]
            (sequence . fmap Just) [1,2,3]
                `shouldBe` Just [1,2,3]
            traverse Just [1,2,3] `shouldBe` Just [1,2,3]
        it "is stronger than Functor and Foldable" $ do
            traverse (Identity . (+1)) [1,2]
                `shouldBe` Identity [2,3]
            runIdentity (traverse (Identity . (+1)) [1,2])
                `shouldBe` [2,3]
            let edgeMap f t = runIdentity (traverse (Identity . f) t)
            edgeMap (+1) [1..5] `shouldBe` [2..6]
        it "can act as fold with Const or Constant" $ do
            let xs = [1,2,3,4,5] :: [Sum Integer]
            traverse (Constant . (+1)) xs
                `shouldBe` Constant (Sum 20)

