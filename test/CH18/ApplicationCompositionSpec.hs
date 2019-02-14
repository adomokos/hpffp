module CH18.ApplicationCompositionSpec where

import Test.Hspec
import Control.Monad ((>=>))

main :: IO ()
main = hspec spec

{-
  Application and Composition did not matter much with Functor and Applicative.
  It just worked:

  fmap id = id
  -- guarantees

  fmap f . fmap g = fmap (f . g)

  With Monad, the situation less neat.

  mcomp :: Monad m =>
           (b -> m c)
        -> (a -> m b)
        ->  a -> m c
  mcomp f g a = f (g a) -- This will have an error

  Kleisli composition:
  We need a function composition written in terms of bind (>>=)
  to allow us to deal with the extra structure, that's what the
  Kleisli fish fives us.

  Let's remind ourselves what ordinary function composition and bind are:

  (.)   ::            (b -> c) -> (a -> b)   -> a -> c
  (>>=) :: Monad m => m a      -> (a -> m b) -> m b

  -- the order is flipped to match >>=
  (>=>)    :: Monad m
           => (a -> m b) -> (b -> m c) -> a -> m c
  flip (.) :: (a -> b)   -> (b -> c)   -> a ->   c
-}

mcomp :: Monad m =>
         (b -> m c)
      -> (a -> m b)
      ->  a -> m c
-- mcomp f g a = join (f <$> (g a)) -- This will work
mcomp f g a = g a >>= f -- join . fmap is bind (>>=)

doubler :: Int -> Maybe Int
doubler x = Just (2*x)

addThree :: Int -> Maybe Int
addThree x = Just (3+x)

spec :: Spec
spec =
  describe "Application and Composition" $ do
    it "works with Functors" $ do
      fmap ((+1) . (+2)) [1..5]
        `shouldBe` ([4..8] :: [Int])
      (fmap (+1) . fmap (+2)) [1..5]
        `shouldBe` ([4..8] :: [Int])
    it "can flip monadic structures" $ do
      let z = doubler >=> addThree
      z 5 `shouldBe` Just 13
