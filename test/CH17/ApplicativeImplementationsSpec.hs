module CH17.ApplicativeImplementationsSpec where

import Test.Hspec

import Control.Applicative
import Data.Monoid ( Sum(..), (<>) )

main :: IO ()
main = hspec spec

-- Exercise: Identity Instance
newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity x = Identity (f x)

-- Exercise: Constant Applicative
newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Show, Ord)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  Constant x <*> Constant y = Constant (x <> y)

spec :: Spec
spec = do
  describe "Identity as Applicative" $
    it "works with lists" $ do
      let result  = const <$> [1,2,3] <*> [4,5,6]
          result' = const <$> Identity [1,2,3] <*> Identity [4,5,6]
      result `shouldBe` [1,1,1,2,2,2,3,3,3]
      result' `shouldBe` Identity [1,2,3]
  describe "Constant as Applicative" $
    it "works with Sum" $ do
      let f = Constant (Sum 1)
          g = Constant (Sum 2)
          result = f <*> g
      result `shouldBe` Constant (Sum 3)
      (pure 1 :: Constant String Int) `shouldBe` Constant ""
