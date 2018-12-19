module CH17.ApplicativesSpec where

import Test.Hspec
import Control.Applicative

main :: IO ()
main = hspec spec

{-
   Applicatives are monoidal functors. The function we are applying
   is also embedded in some structure.

   class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b -- named `apply` or `ap` or `tie fighter`

   Comparing with Functor:
   -- fmap
   (<$>) :: Functor f
         =>   (a -> b) -> f a -> f b

   (<$>) :: Applicative f
         => f (a -> b) -> f a -> f b

   Applicative provides some helper functions:
   liftA  :: Applicative f => (a -> b) -> f a -> f b
   liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
   liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d

   Notice this:
   ($)   ::   (a -> b) ->   a ->   b
   (<$>) ::   (a -> b) -> f a -> f b
   (<*>) :: f (a -> b) -> f a -> f b
-}

spec :: Spec
spec = do
  describe "Applicatives" $ do
    it "function in a context" $ do
      -- good old functor
      fmap (+1) [1,2,3] `shouldBe` [2,3,4]
      pure (+1) <*> [1,2,3] `shouldBe` [2,3,4]
    it "pure - puts it into the contex we are working with" $ do
      (pure 1 :: [Int]) `shouldBe` [1]
      (pure 1 :: Maybe Int) `shouldBe` Just 1
      (pure 1 :: Either String Int) `shouldBe` Right 1
      (pure 1 :: ([Int],Int)) `shouldBe` ([],1)
    it "only applies the function on the right side of the tuple" $
      fmap (+1) (4,5) `shouldBe` (4,6)
  describe "Applicative functors are monoidal functors" $ do
    it "works as such" $ do
      pending
