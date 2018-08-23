{-# LANGUAGE InstanceSigs #-}

module Hpffp.ReaderSpec where

import Test.Hspec
import Control.Applicative (liftA2)
import Control.Monad (join)
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

{-
instance Functor (Reader r) where
    {- fmap :: (a -> b) -}
         {- -> Reader r a -}
         {- -> Reader r b -}
    fmap f (Reader ra) = Reader $ \r -> f (ra r)
-}

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

newtype HumanName =
    HumanName String deriving (Eq, Show)

newtype DogName =
    DogName String deriving (Eq, Show)

newtype Address =
    Address String deriving (Eq, Show)

data Person = Person {
    humanName :: HumanName
  , dogName :: DogName
  , address :: Address } deriving (Eq, Show)

data Dog = Dog {
    dogsName :: DogName
  , dogsAddress :: Address } deriving (Eq, Show)

pers :: Person
pers = Person (HumanName "Big Bird")
              (DogName "Barkley")
              (Address "Sesame Street")

chris :: Person
chris = Person (HumanName "Chris Allen")
               (DogName "Papu")
               (Address "Austin")

-- without Reader
getDog :: Person -> Dog
getDog p = Dog (dogName p) (address p)

-- with Reader
getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address

(<$->>) :: (a -> b) -> (r -> a) -> (r -> b)
(<$->>) = (<$>)

(<*->>) :: (r -> a -> b) -> (r -> a) -> (r -> b)
(<*->>) = (<*>)

getDogR' :: Person -> Dog
getDogR' = Dog <$->> dogName <*> address

getDogR'' :: Person -> Dog
getDogR'' = liftA2 Dog dogName address

getDogRM :: Person -> Dog
getDogRM = do
    name <- dogName
    addy <- address
    return $ Dog name addy

{-
    liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
-}

-- Exercises

-- 1.

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f x y = f <$> x <*> y

-- 2.

asks :: (r -> a) -> Reader r a
asks f = Reader f

instance Functor (Reader a) where
    fmap f (Reader x) = Reader $ f . x

instance Applicative (Reader r) where
    pure a = Reader (\r -> a)
    Reader f <*> Reader g =
        Reader (\r -> f r (g r))

instance Monad (Reader r) where
    return = pure

    (>>=) :: Reader r a
          -> (a -> Reader r b)
          -> (Reader r b)
    (Reader ra) >>= aRb =
        join $ Reader $ \r -> aRb (ra r)

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
    describe "Function as Functor" $ do
        it "has function implementation" $ do
            -- instance Functor ((->) r) where
            --     fmap = (.)
            fmap (+1) (*2) 3 `shouldBe` 7
            (fmap (+1) (*2)) 3 `shouldBe` 7
            ((+1) . (*2)) 3 `shouldBe` 7
            ((+2) . (*1)) 2 `shouldBe` 4
        it "can work with a custom ask function" $ do
            ((runReader ask) (+2)) 3 `shouldBe` 5
    describe "Function as Applicative" $ do
        it "has a function implementation" $ do
            let dog = Dog (DogName "Barkley") (Address "Sesame Street")
            getDog pers `shouldBe` dog
            getDogR pers `shouldBe` dog
            getDogR' pers `shouldBe` dog
            getDogR'' pers `shouldBe` dog
    describe "Exercises" $ do
        it "works with myLiftA2" $ do
            let dog = Dog (DogName "Barkley") (Address "Sesame Street")
            (myLiftA2 Dog dogName address) pers `shouldBe` dog
        it "works with asks" $ do
            runReader (asks (+2)) 3 `shouldBe` 5
    describe "Reader monad" $ do
        it "works for function" $ do
            let dog = Dog (DogName "Barkley") (Address "Sesame Street")
            getDogRM pers `shouldBe` dog
