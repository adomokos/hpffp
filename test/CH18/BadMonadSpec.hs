module CH18.BadMonadSpec where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data CountMe a =
  CountMe Integer a
  deriving (Show, Eq)

instance Functor CountMe where
  fmap f (CountMe i a) =
    CountMe i (f a)

instance Applicative CountMe where
  pure = CountMe 0
  CountMe n f <*> CountMe n' a =
    CountMe (n + n') (f a)

instance Monad CountMe where
  return = pure

  CountMe n a >>= f =
    let CountMe n' b = f a
     in CountMe (n + n') b

instance Arbitrary a
  => Arbitrary (CountMe a) where
  arbitrary =
    CountMe <$> arbitrary <*> arbitrary

instance Eq a => EqProp (CountMe a) where
  (=-=) = eq

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "BadMonadSpec" $ do
    it "does not work with implicit state" $ do
      pendingWith "Dont' run it, too much output"
      let trigger :: CountMe (Int, String, Int)
          trigger = undefined
      quickBatch $ functor trigger
      quickBatch $ applicative trigger
      quickBatch $ monad trigger
