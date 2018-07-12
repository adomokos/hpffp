module FunctorsSpec where

import Test.Hspec
import Test.QuickCheck
import Data.Monoid

main :: IO ()
main = hspec spec

data FixMePls a = FixMe
                | Pls a
                deriving (Eq, Show)

instance Functor FixMePls where
    fmap _ FixMe = FixMe
    fmap f (Pls a) = Pls (f a)

spec :: Spec
spec = do
    describe "Functors" $ do
        it "can be added to types" $
            fmap (+1) (Pls 1) `shouldBe` Pls 2
