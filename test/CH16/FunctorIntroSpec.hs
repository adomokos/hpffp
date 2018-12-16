module CH16.FunctorIntroSpec where

import Test.Hspec

main :: IO ()
main = hspec spec

{-
    Functor is implemented with this type class:

    class Functor f where
      fmap :: (a -> b) -> f a -> f b

    Function signature:
    ($)   :: (a -> b) ->   a ->   b
    (<$>) :: Functor f
          => (a -> b) -> f a -> f b

    ## Functor laws
    1. Idenity:
    fmap id == id

    2. Composition:
    fmap (f . g) = fmap f . fmap g
-}

data FixMePls a =
    FixMe
  | Pls a
  deriving (Eq, Show)

instance Functor FixMePls where
  fmap _ FixMe = FixMe
  fmap f (Pls x) = Pls (f x)

-- 16.6
data WhoCares a =
    ItDoesnt
  | Matter a
  | WhatThisIsCalled
  deriving (Eq, Show)

instance Functor WhoCares where
  fmap _ ItDoesnt = ItDoesnt
  fmap _ WhatThisIsCalled = WhatThisIsCalled
  fmap f (Matter x) = Matter (f x)

-- BAD Example
data CountingBad a =
  Heisenberg Int a
  deriving (Eq, Show)

instance Functor CountingBad where
  fmap f (Heisenberg n a) =
    Heisenberg (n+1) (f a)

-- fix it like this
data CountingGood a =
  Heisenberg' Int a
  deriving (Show, Eq)

instance Functor CountingGood where
  fmap f (Heisenberg' n a) =
    Heisenberg' (n) (f a)

spec :: Spec
spec = do
  describe "Functors" $ do
    it "works with custom type" $ do
      fmap (+1) FixMe `shouldBe` FixMe
      fmap (+1) (Pls 1) `shouldBe` Pls 2
      fmap (++"!") (Pls "Hello") `shouldBe` Pls "Hello!"
  describe "Laws" $ do
    it "abides identity" $ do
      id "Hello" `shouldBe` "Hello"
      fmap id "Hello" `shouldBe` "Hello"
    it "abides composition" $ do
      fmap ((+1) . (*2)) (Just 2) `shouldBe` Just 5
      (fmap (+1) . fmap (*2)) (Just 2) `shouldBe` Just 5
  describe "What matters" $ do
    it "applies to non-nullary" $ do
      fmap (+3) ItDoesnt `shouldBe` ItDoesnt
      fmap (+3) (Matter 2) `shouldBe` Matter 5
      fmap (+3) WhatThisIsCalled `shouldBe` WhatThisIsCalled
  describe "When not abiding the laws" $ do
    it "is misleading" $ do
      let u = "Uncle"
          oneWhoKnocks = Heisenberg 0 u
      fmap (++ " Jesse") oneWhoKnocks
        `shouldBe` Heisenberg 1 "Uncle Jesse"
      let f = ((++ " Jesse") . (++ " lol"))
      fmap f oneWhoKnocks `shouldBe` Heisenberg 1 "Uncle lol Jesse"
      -- Composing the two together
      let j = (++ " Jesse")
          l = (++ " lol")
      (fmap j . fmap l) oneWhoKnocks `shouldBe`
        Heisenberg 2 "Uncle lol Jesse"
      fmap (j . l) oneWhoKnocks `shouldBe`
        Heisenberg 1 "Uncle lol Jesse"
