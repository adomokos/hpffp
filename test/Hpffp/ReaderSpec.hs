module Hpffp.ReaderSpec where

import Test.Hspec
import Control.Applicative (liftA2)
import Data.Char

main :: IO ()
main = hspec spec

boop :: (Num a) => a -> a
boop = (*2)

doop :: (Num a) => a -> a
doop = (+10)

bip :: (Num a) => a -> a
bip = boop . doop

-- Function is the `Functorial` context
bloop :: Integer -> Integer
bloop = fmap boop doop -- functor of functions

-- Function as `Applicative` context
bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop

boopDoop :: Integer -> Integer
boopDoop = do
    a <- boop
    b <- doop
    return (a + b)

-- Exercises
cap :: [Char] -> [Char]
cap = map toUpper

rev :: [Char] -> [Char]
rev = reverse

composed :: [Char] -> [Char]
composed = rev . cap

fmapped :: [Char] -> [Char]
fmapped = fmap cap reverse

tupled :: [Char] -> ([Char], [Char])
tupled = liftA2 (,) cap rev

tupled' :: [Char] -> ([Char], [Char])
tupled' = (,) <$> rev <*> cap

tupled'' :: [Char] -> ([Char], [Char])
tupled'' = do
    x <- rev
    y <- cap
    return (x, y)

tupled''' :: [Char] -> ([Char], [Char])
tupled''' = (,) =<< rev <$> cap

spec :: Spec
spec = do
    describe "Functions" $ do
        it "can be used in `Functorial` context" $ do
            bip 3 `shouldBe` 26
            bloop 3 `shouldBe` 26
        it "can be used in `Applicative` context" $ do
            bbop 3 `shouldBe` 19
            duwop 3 `shouldBe` 19
            ((+) <$> (*2) <*> (+10)) 3 `shouldBe` 19
            boopDoop 3 `shouldBe` 19
        it "works with Strings" $ do
            composed "Julie" `shouldBe` "EILUJ"
            fmapped "Chris" `shouldBe` "SIRHC"
            tupled "Julie" `shouldBe` ("JULIE", "eiluJ")
            tupled' "Julie" `shouldBe` ("eiluJ", "JULIE")
            tupled'' "Julie" `shouldBe` ("eiluJ", "JULIE")
            tupled''' "Julie" `shouldBe` ("EILUJ", "Julie")
