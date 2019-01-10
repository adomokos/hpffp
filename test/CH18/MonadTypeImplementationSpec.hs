module CH18.MonadTypeImplementationSpec where

import Test.Hspec

main :: IO ()
main = hspec spec

data MyMaybe a
  = MyJust a
  | MyNothing
  deriving (Show, Eq)

instance Functor MyMaybe where
  fmap _ MyNothing = MyNothing
  fmap f (MyJust x) = MyJust (f x)

instance Applicative MyMaybe where
  pure = MyJust
  MyNothing <*> _ = MyNothing
  _ <*> MyNothing = MyNothing
  (MyJust f) <*> (MyJust x) = MyJust (f x)

instance Monad MyMaybe where
  return = pure
  MyNothing >>= _ = MyNothing
  MyJust x >>= f = f x

data MyEither a b
  = MyLeft a
  | MyRight b
  deriving (Show, Eq)

instance Functor (MyEither a) where
  fmap _ (MyLeft x) = MyLeft x
  fmap f (MyRight y) = MyRight (f y)

instance Applicative (MyEither a) where
  pure x = MyRight x
  MyLeft x <*> _ = MyLeft x
  _ <*> MyLeft x = MyLeft x
  MyRight f <*> MyRight y = MyRight (f y)

instance Monad (MyEither a) where
  return = pure
  MyLeft x >>= _ = MyLeft x
  MyRight x >>= f = f x

spec :: Spec
spec = do
  describe "Monads for my Maybe" $ do
    let value = MyJust 2
    it "passes Functor" $ do
      (* 2) <$> value `shouldBe` MyJust 4
      (* 2) <$> MyNothing `shouldBe` MyNothing
    it "passes for Applicative" $ do
      (*) <$> value <*> MyJust 2 `shouldBe` MyJust 4
      (*) <$> value <*> MyNothing `shouldBe` MyNothing
    it "passes for Monads" $ do
      ((pure 2 >>= (\x -> MyJust (x * 2))) :: MyMaybe Int) `shouldBe` MyJust 4
  describe "Monads for my Either" $ do
    it "passes for Functor" $ do
      (((*2) <$> MyRight 3) :: MyEither String Int) `shouldBe` MyRight 6
      (((*2) <$> MyLeft "Ho") :: MyEither String Int) `shouldBe` MyLeft "Ho"
    it "passes for Applicative" $ do
      (((*) <$> MyRight 2 <*> MyRight 3) :: MyEither String Int)
        `shouldBe` MyRight 6
      (((*) <$> MyRight 2 <*> MyLeft "Ho") :: MyEither String Int)
        `shouldBe` MyLeft "Ho"
      (((*) <$> MyLeft "Ho" <*> MyRight 2) :: MyEither String Int)
        `shouldBe` MyLeft "Ho"
    it "passes for Monad" $ do
      ((pure 3 >>= (\x -> MyRight (x*2))) :: MyEither String Int)
        `shouldBe` MyRight 6
      ((MyLeft "Ho" >>= (\x -> MyRight (x*2))) :: MyEither String Int)
        `shouldBe` MyLeft "Ho"
