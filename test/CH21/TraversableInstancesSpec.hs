module CH21.TraversableInstancesSpec where

import Test.Hspec

main :: IO ()
main = hspec spec

data TEither a b = TLeft a
                 | TRight b
                 deriving (Show, Eq)

instance Functor (TEither a) where
  fmap _ (TLeft x) = TLeft x
  fmap f (TRight y) = TRight (f y)

instance Applicative (TEither a) where
  pure = TRight
  TLeft x <*> _ = TLeft x
  _ <*> TLeft x = TLeft x
  TRight f <*> TRight x = TRight (f x)

instance Foldable (TEither a) where
  foldMap _ (TLeft _) = mempty
  foldMap f (TRight y) = f y

  foldr _ z (TLeft _) = z
  foldr f z (TRight y) = f y z

instance Traversable (TEither a) where
  traverse _ (TLeft x) = pure (TLeft x)
  traverse f (TRight y) = TRight <$> f y

spec :: Spec
spec = do
  describe "Traversable instances" $ do
    context "MyEither" $ do
      it "works with Functor" $ do
        fmap (+2) (TLeft "fail")
          `shouldBe` (TLeft "fail" :: TEither String Int)
        fmap (+2) (TRight 3)
          `shouldBe` (TRight 5 :: TEither String Int)
      it "works with Applicative" $ do
        (TRight (+2)) <*> (TLeft "fail")
          `shouldBe` (TLeft "fail" :: TEither String Int)
        (TRight (+2)) <*> (TRight 3)
          `shouldBe` (TRight 5 :: TEither String Int)

