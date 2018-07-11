module Hpffp.SemiGroupSpec where

import Test.Hspec
import Test.QuickCheck
import Data.Semigroup

{-
   Semigroup's only law is associativity
-}

-- 1.

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = Trivial

instance Arbitrary Trivial where
    arbitrary = return Trivial

-- 2.

newtype Identity a = Identity a

instance Semigroup a => Semigroup (Identity a) where
    Identity a <> Identity a' = Identity (a <> a')

instance Eq a => Eq (Identity a) where
    Identity a == Identity a' = a == a'

testIdEq :: Eq a => Identity a -> Identity a -> Bool
testIdEq x x' = (x == x') == (\(Identity a) (Identity b) -> a == b) x x'

instance (Show a) => Show (Identity a) where
    show (Identity a) = "Identity " ++ show a

testIdShow :: Show a => Identity a -> Bool
testIdShow x = show x == (\(Identity a) -> "Identity " ++ show a) x

genId :: Arbitrary a => Gen (Identity a)
genId = do
    x <- arbitrary
    return $ Identity x

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = genId

-- 3.

data Two a b = Two a b

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    Two a b <> Two a' b' = Two (a <> a') (b <> b')

instance (Eq a, Eq b) => Eq (Two a b) where
    Two a b == Two a' b' = (a == a') && (b == b')

instance (Show a, Show b) => Show (Two a b) where
    show (Two a b) = "Two " ++ show a ++ " " ++ show b

-- tests
testTwoEq :: (Eq a, Eq b) => Two a b -> Two a b -> Bool
testTwoEq x x' = (x == x') == (\(Two a b) (Two a' b') ->
    (a == a') && (b == b')) x x'

testTwoShow :: (Show a, Show b) => Two a b -> Bool
testTwoShow x = show x ==
                (\(Two a b) -> "Two " ++ show a ++ " " ++ show b) x

-- generators

genTwo :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
genTwo = do
    x <- arbitrary
    y <- arbitrary
    return $ Two x y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = genTwo

-- 4.

data Three a b c = Three a b c

-- instances

instance (Semigroup a, Semigroup b, Semigroup c) =>
    Semigroup (Three a b c) where
        Three a b c <> Three a' b' c' =
            Three (a <> a') (b <> b') (c <> c')

instance (Eq a, Eq b, Eq c) => Eq (Three a b c) where
    Three a b c == Three a' b' c' =
        (a == a') && (b == b') && (c == c')

instance (Show a, Show b, Show c) => Show (Three a b c) where
    show (Three a b c) =
        "Three " ++ show a ++ " " ++ show b ++ " " ++ show c

-- tests

testThreeEq :: (Eq a, Eq b, Eq c) => Three a b c -> Three a b c -> Bool
testThreeEq x x' = (x == x') == (\(Three a b c) (Three a' b' c') ->
                                (a == a') && (b == b') && (c == c')) x x'

testThreeShow x =
    show x == (\(Three a b c) ->
                "Three " ++ show a ++ " " ++ show b ++ " " ++ show c) x


-- generators
genThree :: (Arbitrary a, Arbitrary b, Arbitrary c)
    => Gen (Three a b c)
genThree = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three x y z

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
    Arbitrary (Three a b c) where
        arbitrary = genThree

-- 6.

newtype BoolConj = BoolConj Bool

instance Semigroup BoolConj where
    BoolConj True <> BoolConj True = BoolConj True
    BoolConj _ <> BoolConj _ = BoolConj False

instance Eq BoolConj where
    BoolConj a == BoolConj a' = a == a'

testBCEq :: BoolConj -> BoolConj -> Bool
testBCEq x x' = (x == x') == (\(BoolConj a) (BoolConj b) -> a == b) x x'

-- we must test expected behavior
testBCConj :: BoolConj -> BoolConj -> Bool
testBCConj x x' = (\(BoolConj a) -> a) (x <> x') ==
                  (\(BoolConj a) (BoolConj b) -> a && b) x x'

instance Show BoolConj where
    show (BoolConj a) = "BoolConj " ++ show a

testBCShow :: BoolConj -> Bool
testBCShow x = show x == (\(BoolConj a) -> "BoolConj " ++ show a) x

genBC :: Gen BoolConj
genBC = do
    b <- arbitrary :: Gen Bool
    return $ BoolConj b

instance Arbitrary BoolConj where
    arbitrary = genBC


-- 8. Or Semigroup
data Or a b = Fst a | Snd b

-- Or instances

instance Semigroup (Or a b) where
    Fst _ <> Fst x = Fst x
    Fst _ <> Snd x = Snd x
    Snd x <> Fst _ = Snd x
    Snd x <> Snd _ = Snd x

instance (Show a, Show b) => Show (Or a b) where
    show (Fst x) = "Fst " ++ show x
    show (Snd x) = "Snd " ++ show x

instance (Eq a, Eq b) => Eq (Or a b) where
    Fst _ == Snd _ = False
    Snd _ == Fst _ = False
    Fst x == Fst x' = x == x'
    Snd x == Snd x' = x == x'

-- Or tests

testOr :: (Eq a, Eq b) => Or a b -> Or a b -> Bool
testOr x x' = case (x, x') of
    (Fst _, Fst b) -> x <> x' == Fst b
    (Fst _, Snd b) -> x <> x' == Snd b
    (Snd b, Fst _) -> x <> x' == Snd b
    (Snd b, Snd _) -> x <> x' == Snd b

testOrEq :: (Eq a, Eq b) => Or a b -> Or a b -> Bool
testOrEq x x' = case (x, x') of
    (Fst _, Snd _) -> (x == x') == False
    (Snd _, Fst _) -> (x == x') == False
    (Fst a, Fst b) -> (x == x') == (a == b)
    (Snd a, Snd b) -> (x == x') == (a == b)

testOrShow :: (Show a, Show b) => Or a b -> Bool
testOrShow x = case x of
    Fst a -> show x == "Fst " ++ show a
    Snd a -> show x == "Snd " ++ show a

-- generators
genOr :: (Arbitrary a, Arbitrary b) => Gen (Or a b)
genOr = do
    a <- arbitrary
    b <- arbitrary
    elements [Fst a, Snd b]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
    arbitrary = genOr

-- ===============

semigroupAssoc :: (Eq m, Semigroup m)
               => m -> m -> m -> Bool
semigroupAssoc a b c=
    (a <> (b <> c)) == ((a <> b) <> c)

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool
type S = String
type Id = Identity
type IdentityAssoc = Id S -> Id S -> Id S -> Bool
type IdentityEq = Id S -> Id S -> Bool
type TwoAssoc = Two S S -> Two S S -> Two S S -> Bool
type TwoEq = Two S S -> Two S S -> Bool
type ThreeAssoc = Three S S S -> Three S S S -> Three S S S -> Bool
type ThreeEq = Three S S S -> Three S S S -> Bool
type BC = BoolConj
type BCAssoc = BC -> BC -> BC -> Bool
type OrAssoc = Or Int Char -> Or Int Char -> Or Int Char -> Bool

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Semigroups" $ do
        it "checks TrivialAssoc" $ property $
            (semigroupAssoc :: TrivialAssoc)
        it "checks for Identity" $ property $ do
            _ <- (semigroupAssoc :: IdentityAssoc)
            _ <- (testIdEq :: IdentityEq)
            (testIdShow :: Id String -> Bool)
        describe "Type Two" $ do
            it "checks the Two type assoc" $ property $
                (semigroupAssoc :: TwoAssoc)
            it "checks the Two type equality" $ property $
                (testTwoEq :: TwoEq)
            it "checks the Two type Show" $ property $
                (testTwoShow :: Two Int Float -> Bool)
        describe "Type Three" $ do
            it "checks the assoc" $ property $
                (semigroupAssoc :: ThreeAssoc)
            it "checks equality" $ property $
                (testThreeEq :: ThreeEq)
            it "checks the Show" $ property $
                (testThreeShow :: Three S S S -> Bool)
        describe "BoolConj" $ do
            it "checks the assoc" $ property $
                (semigroupAssoc :: BCAssoc)
            it "checks equality" $ property $ testBCEq
            it "checks Show" $ property $ testBCShow
        describe "Or" $ do
            it "checks the assoc" $ property $
                (semigroupAssoc :: OrAssoc)
            it "checks equality" $ property $ (testOrEq :: Or Int Char -> Or Int Char -> Bool)

