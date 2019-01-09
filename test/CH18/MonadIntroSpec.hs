module CH18.MonadIntroSpec where

import Control.Applicative (liftA)
import Control.Monad (join, liftM, liftM2)
import Test.Hspec

main :: IO ()
main = hspec spec

{-
    ($)   :: (a -> b)   ->   a ->   b
    (<$>) :: (a -> b)   -> f a -> f b
    (<*>) :: f (a -> b) -> f a -> f b
    (>>=) :: m a -> (a -> m b) - > m b

    Three minimally complete Monad instance
    (>>=)  :: m a -> (a -> m b) -> m b
    (>>)   :: m a -> m b -> m b
    return :: a -> m a

    We can force an fmap to work like a monad.
    fmap :: Functor f
         => (a -> f b) -> f a -> f (f b)

    Type of Concat:
    concat :: Foldable t => t [a] -> [a]

    -- or simply
    concat :: [[a]] -> [a]

    -- join is similar to concat
    join :: Monad m => m (m a) -> m a

    Monad also lifts!

    liftA :: Applicative f
          => (a -> b) -> f a -> f b
    liftM :: Monad m
          => (a1 -> r) -> m a1 -> m r
-}
bind :: Monad m => (a -> m b) -> m a -> m b
bind f = join . fmap f

spec :: Spec
spec = do
  describe "Intro to Monads" $ do
    it "works like fmap" $ do
      fmap (+ 1) [1 .. 3] `shouldBe` [2 .. 4]
      ([1 .. 3] >>= return . (+ 1)) `shouldBe` [2 .. 4]
    it "can be simulated with fmaps" $ do
      let addOne x = [x, 1]
      addOne 10 `shouldBe` [10, 1]
      fmap addOne [4, 5, 6] `shouldBe` [[4, 1], [5, 1], [6, 1]]
      concat (fmap addOne [4, 5, 6]) `shouldBe` [4, 1, 5, 1, 6, 1]
    it "works with join and fmap" $ do
      let f = (\x -> Just (x * 2))
      bind f (Just 3) `shouldBe` Just 6
      let f' = (\x -> Nothing) :: (Int -> Maybe Int)
      bind f' (Just 3) `shouldBe` Nothing
      let k = (\x -> [x, 2]) :: (Int -> [Int])
      bind k [3, 4] `shouldBe` [3, 2, 4, 2]
      bind k [] `shouldBe` []
    it "works with liftM just like liftA" $ do
      ((liftM (* 2) (Just 3)) :: Maybe Int) `shouldBe` Just 6
      ((liftA (* 2) (Just 3)) :: Maybe Int) `shouldBe` Just 6
      ((liftM2 (,) (Just 3) (Just 'a')) :: Maybe (Int, Char)) `shouldBe`
                Just (3, 'a')
    it "works as specialized zipWith" $ do
      ((zipWith (+) [3, 4] [5, 6]) :: [Int]) `shouldBe` [8, 10]
      ((liftM2 (+) [3, 4] [5, 6]) :: [Int]) `shouldBe` [8, 9, 9, 10]
