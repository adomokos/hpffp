module Hpffp.ReaderPracticeSpec where

import Test.Hspec
import Control.Applicative
import Data.Maybe

main :: IO ()
main = hspec spec

x = [1,2,3]
y = [4,5,6]
z = [7,8,9]

{- lookup :: Eq a => a -> [(a, b)] -> Maybe b -}
{- lookup = undefined -}

xs :: Maybe Integer
xs = lookup 3 $ zip x y

ys :: Maybe Integer
ys = lookup 6 $ zip y z

zs :: Maybe Integer
zs = lookup 4 $ zip x y

z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 x = (z' x, z' x)

summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool
bolt = (&&) <$> (>3) <*> (<8)

sequA :: Integral a => a -> [Bool]
sequA = sequenceA [(>3), (<8), even]

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)

spec :: Spec
spec = do
    describe "Reader Practices" $ do
        it "can simulate DB calls" $ do
            xs `shouldBe` Just 6
            ys `shouldBe` Just 9
            zs `shouldBe` Nothing
            z' 3 `shouldBe` Just 9
            x1 `shouldBe` Just (6, 9)
            x2 `shouldBe` Nothing
            x3 3 `shouldBe` (Just 9, Just 9)
            summed <$> ((,) <$> xs <*> ys) `shouldBe` Just 15
            fmap summed ((,) <$> xs <*> ys) `shouldBe` Just 15
            bolt 7 `shouldBe` True
            fmap bolt z `shouldBe` [True,False,False]
            (and <$> sequA <$> fromMaybe 0) xs `shouldBe` True
            (and <$> sequA <$> fromMaybe 0) s' `shouldBe` False
            (bolt <$> fromMaybe 0) ys `shouldBe` False
            ((fmap . fmap) bolt z' <$> fromMaybe 0) xs `shouldBe` Nothing

