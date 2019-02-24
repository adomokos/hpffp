module CH20.FoldableInstancesSpec where

import Test.Hspec
import Data.Monoid (Product(..), Sum(..))

main :: IO ()
main = hspec spec

data Identity a = Identity a deriving (Show, Eq)

type PI = Product Integer

instance Foldable Identity where
  foldr f z (Identity x) = f x z
  foldl f z (Identity x) = f z x
  foldMap f (Identity x) = f x

data Optional a = Nada
                | Yep a deriving (Show, Eq)

instance Foldable Optional where
  foldr _ z Nada = z
  foldr f z (Yep x) = f x z

  foldl _ z Nada = z
  foldl f z (Yep x) = f z x

  foldMap _ Nada = mempty -- This needs type-hint with type of Monoid
  foldMap f (Yep a) = f a

spec :: Spec
spec = do
  describe "Creating Foldable Instances" $ do
    it "works with my Identity" $ do
      foldr (*) 1 (Identity 5) `shouldBe` 5
      foldr (*) 5 (Identity 5) `shouldBe` 25
      ((foldMap (*5)) (Identity 100) :: PI)
        `shouldBe` Product 500
    it "works with my Maybe" $ do
      foldr (*) 5 Nada `shouldBe` 5
      foldr (*) 5 (Yep 4) `shouldBe` 20
      foldl (*) 5 Nada `shouldBe` 5
      foldl (*) 5 (Yep 3) `shouldBe` 15
      ((foldMap (*2) Nada) :: Sum Int) `shouldBe` Sum 0
      ((foldMap (*2) Nada) :: Product Int) `shouldBe` Product 1
      ((foldMap (*2) (Yep 3)) :: Sum Int) `shouldBe` Sum 6
