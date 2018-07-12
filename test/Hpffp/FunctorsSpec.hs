module Hpffp.FunctorsSpec where

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

data CountingGood a =
    Heisenberg Int a
    deriving (Show, Eq)

instance Functor CountingGood where
    fmap f (Heisenberg n a) =
        Heisenberg (n) (f a)

replaceWithP :: a -> Char
replaceWithP = const 'p'

spec :: Spec
spec = do
    describe "Functors" $ do
        it "can be added to types" $
            fmap (+1) (Pls 1) `shouldBe` Pls 2
        it "has to ignore state-changing argument" $ do
            let good = Heisenberg 3 "Hello"
            fmap (++"!") good `shouldBe` Heisenberg 3 "Hello!"
        it "works with Maybe values" $ do
            fmap replaceWithP (Just 10) `shouldBe` Just 'p'
            fmap replaceWithP Nothing `shouldBe` Nothing
        it "works with lists" $ do
            -- data [] a = [] | a : [a]
            fmap replaceWithP [1..5] `shouldBe` "ppppp"
            fmap replaceWithP "Ave" `shouldBe` "ppp"
            fmap (+1) [] `shouldBe` []
            fmap replaceWithP [] `shouldBe` []
        it "works with tuples" $ do
            -- data (,) a b = (,) a b
            fmap replaceWithP (10,20) `shouldBe` (10,'p')
            fmap replaceWithP (10,"woo") `shouldBe` (10,'p')
        it "works with functions" $ do
            negate 10 `shouldBe` -10
            let tossEmOne = fmap (+1) negate
            tossEmOne 10 `shouldBe` -9
            tossEmOne (-10) `shouldBe` 11
    describe "Functors are stacked" $ do
        let n = Nothing
            w = Just "woohoo"
            ave = Just "Ave"
            lms = [ave, n, w]
        it "replaces lists" $ do
            replaceWithP lms `shouldBe` 'p'
            fmap replaceWithP lms `shouldBe` "ppp"
        it "can lift twice" $
            (fmap . fmap) replaceWithP lms
                `shouldBe` [Just 'p', Nothing, Just 'p']
        it "can lift three times" $
            (fmap . fmap . fmap) replaceWithP lms
                `shouldBe` [Just "ppp", Nothing, Just "pppppp"]
        it "can list 4 times" $ do
            let ha = Just ["Ha", "Ha"]
                lmls = [ha, Nothing, Just []]
            (fmap . fmap) replaceWithP lmls
                `shouldBe` [Just 'p', Nothing, Just 'p']
            (fmap . fmap . fmap) replaceWithP lmls
                `shouldBe` [Just "pp", Nothing, Just []]
            (fmap . fmap . fmap . fmap) replaceWithP lmls
                `shouldBe` [Just ["pp","pp"], Nothing, Just []]
