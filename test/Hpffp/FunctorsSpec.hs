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

replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP' = replaceWithP

liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP

liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace

n :: Maybe String
n = Nothing

w :: Maybe String
w = Just "woohoo"

ave :: Maybe String
ave = Just "Ave"

lms :: [Maybe String]
lms = [ave, n, w]

twiceLifted :: (Functor f1, Functor f) => f (f1 a) -> f (f1 Char)
twiceLifted = (fmap . fmap) replaceWithP

twiceLifted' :: [Maybe [Char]] -> [Maybe Char]
twiceLifted' = twiceLifted

thriceLifted :: (Functor f2, Functor f1, Functor f)
    => f (f1 (f2 a)) -> f (f1 (f2 Char))
thriceLifted = (fmap . fmap . fmap) replaceWithP

thriceLifted' :: [Maybe [Char]] -> [Maybe [Char]]
thriceLifted' = thriceLifted

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
    describe "More stacking" $ do
        it "replaces with more specific P" $
            replaceWithP' lms `shouldBe` 'p'
        it "replaces with lift" $ do
            liftedReplace lms `shouldBe` "ppp"
        it "replaces with more specific typed function" $ do
            liftedReplace' lms `shouldBe` "ppp"
        it "replaces with twiceLifted" $ do
            twiceLifted lms `shouldBe` [Just 'p', Nothing, Just 'p']
        it "replaces with twiceLifted'" $ do
            twiceLifted' lms `shouldBe` [Just 'p', Nothing, Just 'p']
        it "replaces with thriceLifted" $ do
            thriceLifted lms `shouldBe` [Just "ppp", Nothing, Just "pppppp"]
        it "replaces with thriceLifted'" $ do
            thriceLifted' lms `shouldBe` [Just "ppp", Nothing, Just "pppppp"]

