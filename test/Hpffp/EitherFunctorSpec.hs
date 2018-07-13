module Hpffp.EitherFunctorSpec where

import Test.Hspec

main :: IO ()
main = hspec spec

incIfRight :: Num a => Either e a -> Either e a
incIfRight (Left e) = Left e
incIfRight (Right n) = Right (n+1)

showIfRight :: Show a => Either e a -> Either e String
showIfRight (Left e) = Left e
showIfRight (Right s) = Right $ show s

incEither :: Num a => Either e a -> Either e a
incEither = fmap (+1)

showEither :: Show a => Either e a -> Either e String
showEither = fmap show

liftedInc :: (Functor f, Num b) => f b -> f b
liftedInc = fmap (+1)

liftedShow :: (Functor f, Show a) => f a -> f String
liftedShow = fmap show

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
    fmap _ (First a) = First a
    fmap f (Second b) = Second (f b)

spec :: Spec
spec = do
    describe "Either Functors" $ do
        it "works with specific function" $ do
            incIfRight ((Right 3) :: Either String Int)
                `shouldBe` Right 4
            incIfRight ((Left "oops") :: Either String Int)
                `shouldBe` Left "oops"
            showIfRight ((Left "oops") :: Either String Int)
                `shouldBe` Left "oops"
            showIfRight ((Right 3) :: Either String Int)
                `shouldBe` Right "3"
        it "works with fmap" $ do
            incEither ((Right 3) :: Either String Int)
                `shouldBe` Right 4
            incEither ((Left "oops") :: Either String Int)
                `shouldBe` Left "oops"
            showEither ((Left "oops") :: Either String Int)
                `shouldBe` Left "oops"
            showEither ((Right 3) :: Either String Int)
                `shouldBe` Right "3"
        it "works when lifted" $ do
            liftedInc ((Right 3) :: Either String Int)
                `shouldBe` Right 4
            liftedInc ((Left "oops") :: Either String Int)
                `shouldBe` Left "oops"
            liftedShow ((Left "oops") :: Either String Int)
                `shouldBe` Left "oops"
            liftedShow ((Right 3) :: Either String Int)
                `shouldBe` Right "3"
        it "works with the custom data type" $ do
            liftedInc ((Second 3) :: Sum String Int)
                `shouldBe` Second 4
            liftedInc ((First "oops") :: Sum String Int)
                `shouldBe` First "oops"
            liftedShow ((First "oops") :: Sum String Int)
                `shouldBe` First "oops"
            liftedShow ((Second 3) :: Sum String Int)
                `shouldBe` Second "3"
