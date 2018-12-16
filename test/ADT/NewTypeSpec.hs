{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
module ADT.NewTypeSpec where

import Test.Hspec
import Data.Int

main :: IO ()
main = hspec spec

{-
   One key contrast between a __newtype__ and a type alias is that you can
   define typeclass instances for newtype(s) that differ from the instances
   for their underlying type.

   -XGeneralizedNewtypeDeriving language extension:
   Tells the compiler to allow our newtype to relo on a typeclass instance
   for the type it contains.
-}

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats = Goats Int deriving (Eq, Show, TooMany)
-- This won't work "parse error on input ‘deriving’"
-- type Goats = Goats Int deriving (Eq, Show, TooMany)

-- With `GeneralizedNewtypeDeriving` we don't need to
-- "manually" implement the typeclass, we can rely on
-- using the embadded type.
{-
instance TooMany Goats where
  tooMany (Goats n) = tooMany n
-}

-- Exercises: Logic Goats
-- 1.
instance TooMany (Int, String) where
  tooMany (x, _) = tooMany x

-- 2.
-- instance TooMany (Int, Int) where
  -- tooMany (x, y) = tooMany (x+y)

-- 3.
instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (x, y) = tooMany (x+y)

{-
   Sum Types

  data Bool = False | True
  The "|" represents logical disjunction.

  Product Types
  Product type's cardinality is the product of the cardinalities of its inhabitants.
  For someone familiar with C, Product is like a structure.
-}

data NumberOrBool = Numba Int8
                  | BoolyBool Bool
                  deriving (Show, Eq)

spec :: Spec
spec = 
  describe "ADT - newtype" $ do
    it "works with them" $ do
      tooMany (42 :: Int) `shouldBe` False
      tooMany (43 :: Int) `shouldBe` True
      tooMany (Goats 42) `shouldBe` False
      tooMany (Goats 43) `shouldBe` True
    it "works with the exercises" $ do
      tooMany (42 :: Int, "Hello") `shouldBe` False
      tooMany (43 :: Int, "Hello") `shouldBe` True
      tooMany ((40, 2) :: (Int, Int)) `shouldBe` False
      tooMany ((40, 3) :: (Int, Int)) `shouldBe` True
