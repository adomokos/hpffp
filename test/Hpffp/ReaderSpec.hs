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

{-
    (.) :: (b -> c) -> (a -> b) -> a -> c
    -- or perhaps
    (.) :: (b -> c) -> (a -> b) -> (a -> c)

    -- How does this line up with Functor?
    (.) :: (b -> c) -> (a -> b) -> (a -> c)
    fmap :: Functor f => (a -> b) -> f a -> f b

    -- Remove the name of the functions

    :: (b -> c) -> (a -> b) -> (a -> c)
    :: (a -> b) ->     f a  ->     f b

    -- Changing up the letters
    -- Without changing the meaning

    :: (b -> c) -> (a -> b) -> (a -> c)
    :: (b -> c) ->     f b  ->     f c

    -- f is ((->) a)
    :: (b -> c) -> (a    -> b) -> (a   -> c)
    :: (b -> c) -> ((->) a) b -> ((->) a) c

    -- Unroll the prefix notation into infix

    :: (b -> c) -> (a -> b) -> (a -> c)
    :: (b -> c) -> (a -> b) -> (a -> c)

    -- Bada ding. Functorial lifting for functions.
-}

newtype Reader r a =
    Reader { runReader :: r -> a }

instance Functor (Reader r) where
    {- fmap :: (a -> b) -}
         {- -> Reader r a -}
         {- -> Reader r b -}
    fmap f (Reader ra) = Reader $ \r -> f (ra r)

-- same as (.)
compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g = \x -> f (g x)

{-
    See it?
    \r -> f (ra r)
    \x -> f (g  x)

    We can use the fact that this is function composition.

instance Functor (Reader r) where
    fmap f (Reader ra) = Reader $ (f . ra)
-}

ask :: Reader a a
ask = Reader id

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
    describe "Functor class" $ do
        it "has function implementation" $ do
            -- instance Functor ((->) r) where
            --     fmap = (.)
            fmap (+1) (*2) 3 `shouldBe` 7
            (fmap (+1) (*2)) 3 `shouldBe` 7
            ((+1) . (*2)) 3 `shouldBe` 7
            ((+2) . (*1)) 2 `shouldBe` 4
        it "can work with a custom ask function" $ do
            ((runReader ask) (+2)) 3 `shouldBe` 5
