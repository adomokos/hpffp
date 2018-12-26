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

-- Maybe applicative exercise
validateLength :: Int -> String -> Maybe String
validateLength maxLen s =
  if (length s) > maxLen
     then Nothing
     else Just s

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s =
  fmap Name $ validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress a =
  fmap Address $ validateLength 100 a

data Person = Person Name Address deriving (Eq, Show)

mkPerson :: String -> String -> Maybe Person
mkPerson n a =
  Person <$> mkName n <*> mkAddress a

-- Build Maybe for Functor/Applicative
data AMaybe a = ANothing | AJust a deriving (Show, Eq)

instance Functor AMaybe where
  fmap _ ANothing = ANothing
  fmap f (AJust x) = AJust (f x)

instance Applicative AMaybe where
  pure = AJust
  _ <*> ANothing = ANothing
  ANothing <*> _ = ANothing
  AJust f <*> AJust x = AJust (f x)

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
  describe "Exercise: Fixer Upper" $ do
    it "works as expected" $ do
      let r = const <$> Just "Hello" <*> Just "World"
      r `shouldBe` Just "Hello"
      let x = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> Just [1,2,3]
      x `shouldBe` Just (90, 10, "Tierness", [1,2,3])
  describe "AMaybe - custom type functor and applicative" $
    it "works for both" $ do
      fmap (*2) (AJust 3) `shouldBe` AJust 6
      fmap (*2) ANothing `shouldBe` ANothing
      (*) <$> AJust 2 <*> AJust 3 `shouldBe` AJust 6
      (*) <$> AJust 2 <*> ANothing `shouldBe` ANothing
      (*) <$> ANothing <*> AJust 3 `shouldBe` ANothing
