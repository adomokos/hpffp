module CH16.TransformingUnappliedSpec where

import Test.Hspec

main :: IO ()
main = hspec spec

data Two a b =
  Two a b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

data Or a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Or a) where
  fmap _ (First x) = First x
  fmap f (Second y) = Second (f y)


spec :: Spec
spec = do
  describe "Transforming unapplied arguments" $ do
    it "needs * -> *" $ do
      let t = Two "hello" 3
      fmap (+2) t `shouldBe` Two "hello" 5
      let first = First "hello"
      fmap (++"!") first `shouldBe` first
      let second = Second 3
      fmap (+3) second `shouldBe` (Second 6 :: Or String Int)
