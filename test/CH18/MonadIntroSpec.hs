module CH18.MonadIntroSpec where

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
-}

spec :: Spec
spec = do
  describe "Intro to Monads" $ do
    it "works like fmap" $ do
      fmap (+1) [1..3] `shouldBe` [2..4]
      ([1..3] >>= return . (+1)) `shouldBe` [2..4]
