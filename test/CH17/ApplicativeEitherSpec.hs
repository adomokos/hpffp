module CH17.ApplicativeEitherSpec where

import Test.Hspec

main :: IO ()
main = hspec spec

data Validation err a
  = Failure err
  | Success a
  deriving (Show, Eq)

instance Functor (Validation err) where
  fmap _ (Failure err) = Failure err
  fmap f (Success x) = Success (f x)

instance Monoid err => Applicative (Validation err) where
  pure x = Success x
  Failure err <*> Success x = Failure err
  Success x <*> Failure err = Failure err
  Failure err <*> Failure err' = Failure (err `mappend` err')
  Success f <*> Success x = Success (f x)

validToEither :: Validation e a -> Either e a
validToEither (Failure err) = Left err
validToEither (Success a) = Right a

eitherToValid :: Either e a -> Validation e a
eitherToValid (Left err) = Failure err

eitherToValie (Right a) = Success a

-- eitherToValid . validToEither == id
-- validToEither . eitherToValid == id
data Errors
  = DividedByZero
  | StackOverflow
  | MooglesChewedWires
  deriving (Eq, Show)

spec :: Spec
spec =
  describe "Applicative with Either" $ do
    it "works as expected" $ do
      (pure 1 :: Either String Int) `shouldBe` Right 1
      (Right (+ 1) <*> Right 1 :: Either String Int) `shouldBe` Right 2
      (Right (+ 1) <*> Left ":(" :: Either String Int) `shouldBe` Left ":("
      (Left ":(" <*> Right 1 :: Either String Int) `shouldBe` Left ":("
      (Left ":(" <*> Left "sadface.png" :: Either String Int) `shouldBe`
        Left ":("
    it "uses Monoid to combine them" $ do
      let success = (Success (+ 1) <*> Success 1) :: Validation String Int
      success `shouldBe` Success 2
      let failure = Success (+ 1) <*> Failure [StackOverflow]
      failure `shouldBe` Failure [StackOverflow]
      let failure' =
            (Failure [StackOverflow] <*> Success (+ 1)) :: Validation [Errors] Int
      failure' `shouldBe` Failure [StackOverflow]
      let failures =
            Failure [MooglesChewedWires] <*> Failure [StackOverflow] :: Validation [Errors] Int
      failures `shouldBe` Failure [MooglesChewedWires, StackOverflow]
