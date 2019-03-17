{-# LANGUAGE InstanceSigs #-}
module CH22.ReaderIntroSpec where

import Test.Hspec
import Control.Applicative
import Control.Monad
import Data.Char

boop :: Int -> Int
boop = (*2)

doop :: Int -> Int
doop = (+10)

bip :: Int -> Int
bip = boop . doop

-- | Partially applied function's Functor instance
bloop :: Int -> Int
bloop = fmap boop doop

{-
   Similar to `fmap boop doop x == (*2) ((+10) x)`
   This is the functor of functions.
-}

-- | The argument will be passed to both functions in parallel with Applicative

bbop :: Int -> Int
bbop = (+) <$> boop <*> doop

duwop :: Int -> Int
duwop = liftA2 (+) boop doop

-- | Monadic Implementation
boopDoop :: Int -> Int
boopDoop = do
  a <- boop
  b <- doop
  return (a + b)

{-
  What we've seen here is that we can have a Functor, Applicative and
  Monad for partially applied functions. In all cases, these are awaiting
  application to one argument that will allow both functions to be evaluated.
  The Functor is function composition, the `Applicative` and `Monad` chain the
  argument forward in addition to the composition.

  This is the idea of `Reader`.
  It it a was of stringing function together when all those functions are awaiting
  one input from a shared environment.
-}

-- Warming up exercise

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

-- We could compose them with (.) or fmap

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> rev <*> cap

{-
  Functor for functions:
  (.)  ::              (b -> c) -> (a -> b) -> (a -> c)
  fmap :: Functor f => (a -> b) -> f a      -> f b

  -- changing up the letters
  (.)  ::              (b -> c) -> (a -> b) -> (a -> c)
  fmap :: Functor f => (b -> c) -> f b      -> f c

  -- or
  (b -> c) -> (a -> b) -> (a -> c)
  (b -> c) -> ((->) a) b -> ((->) a) c

  -- f is ((->) a)
-}

-- Reader?

newtype Reader r a =
  Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader ra) =
    Reader $ f . ra

instance Applicative (Reader r) where
  pure x = Reader $ \_e -> x
  (Reader f) <*> (Reader x) = Reader $ \e -> (f e) (x e)

instance Monad (Reader r) where
  return = pure
  x >>= f = Reader $ \e -> runReader (f (runReader x e)) e

ask :: Reader a a
ask = Reader id

asks :: (e -> a) -> Reader e a
-- asks f = do
  -- e <- ask
  -- return $ f e
-- more elegantly
asks f = fmap f ask

-- local transforms the environment a Reader sees
local :: (e -> t) -> Reader t a -> Reader e a
-- local f r = do
  -- e <- ask
  -- pure $ runReader r (f e)
-- more elegantly
local f r = fmap (\e -> runReader r (f e)) ask

fnReader :: a -> Reader e (a, e)
fnReader x = do
  e <- ask
  pure $ (x, e)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Partially applied Functions" $ do
    it "has Functor instances" $
      bloop 3 `shouldBe` 26
    it "has Applicative instances" $
      bbop 3 `shouldBe` 19
    it "needs two arguments" $ do
      let fn = (+) <$> (*2)
      fn 3 6 `shouldBe` 12
    it "has Monadic instances" $
      boopDoop 3 `shouldBe` 19
  describe "Compose two String functions" $ do
    it "reverses and ucases strings" $
      composed "John" `shouldBe` "NHOJ"
    it "uses Applicative to run fns in parallel" $
      tupled "John" `shouldBe` ("nhoJ", "JOHN")

  describe "Reader" $ do
    it "works as Functor" $ do
      let readerApp = fmap (+2) (Reader (*3))
      runReader readerApp 3 `shouldBe` 11
    it "has an ask function" $ do
      (runReader . Reader) (+1) 1234
        `shouldBe` 1235
      (runReader (pure "Banana") 1234)
        `shouldBe` "Banana"
    it "extracts info from environment" $ do
      let x = runReader (fnReader 10) 20
      x `shouldBe` (10,20)
    it "can work with different examples" $ do
      let x = runReader (pure 10 >>= fnReader) 20
      x `shouldBe` (10,20)
      let y = runReader (ask >>= fnReader) 20
      y `shouldBe` (20,20)
      let z = runReader (liftM (*10) ask) 20
      z `shouldBe` 200
      let v = runReader (liftM2 (,) ask ask) 20
      v `shouldBe` (20,20)
    it "can provide Applicative" $ do
      let result = runReader ((,) <$> (fnReader 10) <*> (fnReader 100)) 20
      result `shouldBe` ((10,20),(100,20))
