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
