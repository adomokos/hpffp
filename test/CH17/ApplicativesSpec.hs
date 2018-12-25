module CH17.ApplicativesSpec where

import Test.Hspec
import Control.Applicative
import Data.Monoid

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

   Tuple monoid and Applicative side by side

   instance (Monoid a, Monoid b) => Monoid (a,b) where
     mempty = (mempty, mempty)
     (a,b) `mappend` (a', b') =
       (a `mappend` a', b `mappend` b')

   instance (Monoid a) => Applicative ((,) a) where
     pure x = (mempty, x)
     (u, f) <*> (v, x) =
       (u `mappend` v, f x)

    ## Maybe Monoid and Applicative

    instance Monoid a => Monoid (Maybe a) where
      mempty = Nothing
      mappend m Nothing = m
      mappend Nothing m = m
      mappend (Just a) (Just a') =
        Just (a `mappend` a')

    instance Applicative Maybe where
      pure = Just

      Nothing <*> _ = Nothing
      _ <*> Nothing = Nothing
      Just f <*> Just x = Just (f x)

    17.5 Applicative in Use

    ~List Applicative~

    -- f - []
    (<*>) :: f (a -> b) -> f a -> f b
    (<*>) :: [] (a -> b) -> [] a -> [] b

    -- more syntactically typical
    (<*>) :: [(a -> b)] ->  [a] ->  [b]

    pure :: a ->  f a
    pure :: a -> [] a
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
      [(*2),(*3)] <*> [4,5] `shouldBe` [8,10,12,15]
      Just (*2) <*> Just 2 `shouldBe` Just 4
      Just (*2) <*> Nothing `shouldBe` Nothing
      Nothing <*> Just 2 `shouldBe` (Nothing :: Maybe Int)
      Nothing <*> Nothing `shouldBe` (Nothing :: Maybe Int)
      ("Woo", (+1)) <*> (" Hoo!", 2)
        `shouldBe` ("Woo Hoo!", 3)
    it "combines monoid values" $ do
      (Sum 2, (+1)) <*> (Sum 1, 2) `shouldBe` (Sum 3, 3)
      (Product 3, (+9)) <*> (Product 2, 8)
        `shouldBe` (Product 6, 17)
      (All True, (+1)) <*> (All False, 2) `shouldBe` (All False, 3)
  describe "17.5 Applicative in Use" $ do
    it "works with Lists as Functors" $ do
      fmap (2^) [1,2,3] `shouldBe` [2,4,8]
      fmap (^2) [1,2,3] `shouldBe` [1,4,9]
    it "works with Lists as Applicatives" $ do
      [(+1),(*2)] <*> [2,4] `shouldBe` [3,5,4,8]
      (,) <$> [1,2] <*> [3,4] `shouldBe` [(1,3),(1,4),(2,3),(2,4)]
      -- liftA2 is the same
      liftA2 (,) [1,2] [3,4] `shouldBe` [(1,3),(1,4),(2,3),(2,4)]
      (+) <$> [1,2] <*> [3,5] `shouldBe` [4,6,5,7]
      liftA2 (+) [1,2] [3,5] `shouldBe` [4,6,5,7]
      max <$> [1,2] <*> [1,4] `shouldBe` [1,4,2,4]
      liftA2 max [1,2] [1,4] `shouldBe` [1,4,2,4]
