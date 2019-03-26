module CH22.FunctionMonadSpec where

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

barPlus :: (Foldable t, Num a, Functor t)
        => t a
        -> (t a, Int)
barPlus r = (foo r, length r)

frooty :: (Num a, Functor f, Foldable f)
       => f a
       -> (f a, Int)
frooty r = bar (foo r) r

-- Make it more like reader-y
frooty' :: Num a => [a] -> ([a], Int)
frooty' = \r -> bar (foo r) r

-- fooBind :: (t2 -> t1)
        -- -> (t1 -> t2 -> t)
        -- -> t2
        -- -> t
fooBind :: (r -> a)
        -> (a -> r -> b)
        -> (r -> b)
fooBind m k = \r -> k (m r) r

-- Looks like:
{-
(>>=) :: Monad m =>
   m    a  -> (a -> (m    b)) -> m  b
  (r -> a) -> (a -> (r -> b)) -> (r -> b)

  -- this is how we get `((->) r)` as our
  -- `m` structure
-}

spec :: Spec
spec = do
  describe "Functions are Monads" $ do
    it "works with the examples" $ do
      let r = foo (*2)
      foo (*2) `shouldBe` 7
      bar 4 [1..3] `shouldBe` (4, 3)
      froot [1..3] `shouldBe` ([2..4], 3)
      barOne [1..3] `shouldBe` ([1..3], 3)
      barPlus [1..3] `shouldBe` ([2..4], 3)
      frooty [1..3] `shouldBe` ([2..4], 3)
      frooty' [1..3] `shouldBe` ([2..4], 3)
      -- fooBind foo [1..3] `shouldBe` ([2..4], 3)
