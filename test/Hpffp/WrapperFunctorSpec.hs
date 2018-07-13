module Hpffp.WrapperFunctorSpec where

import Test.Hspec

main :: IO ()
main = hspec spec

data Wrap f a =
    Wrap (f a)
    deriving (Eq, Show)

instance Functor f => Functor (Wrap f) where
    fmap f (Wrap fa) = Wrap (fmap f fa)

spec :: Spec
spec = do
    describe "Wrapper Functors" $ do
        it "can wrap types" $ do
            fmap (+1) (Wrap (Just 1))
                `shouldBe` Wrap (Just 2)
            fmap (+1) (Wrap [1,2,3])
                `shouldBe` Wrap [2..4]
