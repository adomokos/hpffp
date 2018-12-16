module CH16.ChapterExercisesSpec where

import Test.Hspec

main :: IO ()
main = hspec spec

{-
  1. data Bool = False | True
  No Functor instance can be declared, as it's kind is `Bool :: *`
-}

-- 2.
data BoolAndSomethingElse a =
  False' a | True' a
  deriving (Show, Eq)

instance Functor (BoolAndSomethingElse) where
  fmap f (False' x) = False' (f x)
  fmap f (True' x) = True' (f x)

-- 3.
data BoolAndMaybeSomethingElse a =
  Falsish | Truish a
  deriving (Show, Eq)

instance Functor (BoolAndMaybeSomethingElse) where
  fmap _ Falsish = Falsish
  fmap f (Truish x) = Truish (f x)

-- 4.
newtype Mu f = InF { outF :: f (Mu f) } -- deriving (Show, Eq)

-- instance Functor Mu where
  -- fmap f (InF x) = Inf (f x)



spec :: Spec
spec = do
  describe "Chapter Exercises" $ do
    it "2. works for BoolAndSomethingElse" $ do
      let f = (False' 12 :: BoolAndSomethingElse Int)
      fmap (+3) f `shouldBe` False' 15
      let t = True' "Hello"
      fmap (++"!") t `shouldBe` True' "Hello!"
    it "3. works for BoolAndMaybeSomethingElse" $ do
      fmap (+2) (Falsish :: BoolAndMaybeSomethingElse Int) `shouldBe` Falsish
      fmap (+2) (Truish 3 :: BoolAndMaybeSomethingElse Int) `shouldBe` Truish 5
    it "4. works with Mu" $ do
      pending
