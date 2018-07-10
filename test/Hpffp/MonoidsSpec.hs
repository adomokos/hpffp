module Hpffp.MonoidsSpec where

import Test.Hspec
import Test.QuickCheck
import Data.Monoid

main :: IO ()
main = hspec spec

data Optional a =
    Nada
  | Only a
  deriving (Show, Eq)

instance Monoid a => Monoid (Optional a) where
    mempty = Nada

    mappend Nada (Only x) = Only x
    mappend (Only x) Nada = Only x
    mappend (Only x) (Only y) = Only (x `mappend` y)

-- sample (genOnly :: Gen (Optional String))
genOnly :: Arbitrary a => Gen (Optional a)
genOnly = do
  x <- arbitrary
  return $ Only x

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary =
    frequency [ (1, genOnly)
              , (1, return Nada) ]

newtype First' a =
    First' { getFirst' :: Optional a }
    deriving (Eq, Show)

instance Monoid (First' a) where
    mempty = First' Nada
    mappend (First' Nada) (First' Nada) = First' Nada
    mappend (First' Nada) (First' (Only x)) = First' (Only x)
    mappend (First' (Only x)) (First' Nada) = First' (Only x)
    mappend (First' (Only x)) (First' (Only _)) = First' (Only x)

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String
                 -> First' String
                 -> First' String
                 -> Bool

genFirst :: Arbitrary a => Gen (First' a)
genFirst = do
    x <- arbitrary
    return (First' { getFirst' = x })

instance Arbitrary a => Arbitrary (First' a) where
    arbitrary = genFirst

data Bull =
      Fools
    | Twoo
    deriving (Eq, Show)

instance Arbitrary Bull where
    arbitrary =
        frequency [ (1, return Fools)
                  , (1, return Twoo) ]

instance Monoid Bull where
    mempty = Fools
    mappend _ _ = Fools

type BullMappend =
    Bull -> Bull -> Bull -> Bool

monoidAssoc :: (Eq m, Monoid m)
    => m -> m -> m -> Bool
monoidAssoc a b c =
    (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

spec :: Spec
spec = do
    describe "Number Monoids" $ do
        it "works with Sum and Product types" $ do
            let result = mappend (Sum 1) (Sum 5)
            getSum result `shouldBe` 6
            let result = mappend (Product 5) (Product 5)
            getProduct result `shouldBe` 25
            let result = mappend (Sum 4.5) (Sum 3.4)
            getSum  result `shouldBe` 7.9
        it "can join multiple items" $ do
            let result = mappend mempty Sum 9
            getSum result `shouldBe` 9
            let result = (Sum 8) <> (Sum 9) <> (Sum 10)
            getSum result `shouldBe` 27
            let result = mconcat [(Sum 8), (Sum 9), (Sum 10)]
            getSum result `shouldBe` 27
        it "can be used in folds" $ do
            let result = foldr mappend mempty ([2, 4, 6] :: [Product Int])
            getProduct result `shouldBe` 48
            let result = foldr mappend mempty ([2, 4, 6] :: [Sum Int])
            getSum result `shouldBe` 12
            let result = foldr mappend mempty ["blah", "woot"]
            result `shouldBe` "blahwoot"
    describe "Boolean Monoids" $ do
        it "defines All" $ do
            let result = All True <> All True
            getAll result `shouldBe` True
            let result = All True <> All False
            getAll result `shouldBe` False
        it "defines Any" $ do
            let result = Any True <> Any False
            getAny result `shouldBe` True
            let result = Any False <> Any False
            getAny result `shouldBe` False
    describe "Maybe Monoids" $ do
        it "defines First" $ do
            let result = First (Just 1) `mappend` First (Just 2)
            getFirst result `shouldBe` Just 1
            let result = First Nothing <> First (Just 2)
            getFirst result `shouldBe` Just 2
            let result = First Nothing <> First Nothing
            getFirst result `shouldBe` (Nothing :: Maybe Int)
        it "defines Last" $ do
            let result = Last (Just 1) `mappend` Last (Just 2)
            getLast result `shouldBe` Just 2
            let result = Last (Just 1) <> Last Nothing
            getLast result `shouldBe` Just 1
            let result = Last Nothing <> Last Nothing
            getLast result `shouldBe` (Nothing :: Maybe Int)
    describe "Optional with Monoids" $ do
        it "works with monoid operations" $ do
            let (Only result) = Only (Sum 1) `mappend` Only (Sum 1)
            getSum result `shouldBe` 2
            let (Only result) = Only (Sum 1) <> Nada
            getSum result `shouldBe` 1
            let (Only result) = Only [1] `mappend` Nada
            result `shouldBe` [1]
            let (Only result) = Nada `mappend` Only (Sum 1)
            getSum result `shouldBe` 1
    describe "Identity" $ do
        let myList = [1..424]
        it "uses 0 for identity for addition" $
            map (+0) myList `shouldBe` myList
        it "can't use 0 for identity for multiplication" $
            map (*0) myList `shouldNotBe` myList
        it "uses 1 for identity for multiplication" $
            map (*1) myList `shouldBe` myList
        it "can't use 1 for identity for addition" $
            map (+1) myList `shouldNotBe` myList
    describe "Monoids with QuickCheck" $ do
        it "is associative for addition" $ property $
            \a b c -> a + (b + c) == (a + b) + (c :: Integer)
        it "is associative for monoids" $ property $ 
            (monoidAssoc :: String -> String -> String -> Bool)
        it "checks for left Identity" $ property $ do
            (monoidLeftIdentity :: String -> Bool)
        it "checks for right Identity" $ property $ do
            (monoidRightIdentity :: String -> Bool)
    describe "Testing QuickCheck patience" $ do
        let ma = (monoidAssoc :: BullMappend)
            {- mli = (monoidLeftIdentity :: Bull -> Bool) -}
            {- mlr = (monoidRightIdentity :: Bull -> Bool) -}
        it "checks for mappend" $ property $ ma
        {- it "checks for left identity" $ property $ mli -}
    describe "Monoid examples" $ do
        it "checks for associative" $ property $ (monoidAssoc :: FirstMappend)
        it "checks for left identity" $ property $ (monoidLeftIdentity :: First' String -> Bool)
        it "checks for right identity" $ property $ (monoidRightIdentity :: First' String -> Bool)
