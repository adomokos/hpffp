module CH17.ChapterExercisesSpec where

import Control.Applicative
import Data.Monoid
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

main :: IO ()
main = hspec spec

{-

  1.
  pure :: a -> [a]
  (<*>) :: [(a -> b)] -> [a] -> [b]

  2.
  pure :: a -> IO a
  (<*>) :: IO (a -> b) -> IO a -> IO b

  3.
  (,) a

  pure :: a -> (,) a
  (<*>) :: (,) (a -> b) -> (,) a -> (,) b

  4.
  (->) e

  pure :: a -> (->) a
  (<*>) :: (->) (a -> b) -> (->) a -> (->) b
-}
-- 1.
data Pair a =
  Pair a
       a
  deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure x = Pair x x
  Pair f f' <*> Pair x y = Pair (f x) (f' y)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = genPair

genPair :: Arbitrary a => Gen (Pair a)
genPair = do
  a <- arbitrary
  a' <- arbitrary
  return $ Pair a a'

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

-- 2.
data Two a b =
  Two a
      b
  deriving (Show, Eq)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance Monoid a => Applicative (Two a) where
  pure = Two mempty
  Two m f <*> Two m' x = Two (m <> m') (f x)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = genTwo

genTwo :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
genTwo = do
  a <- arbitrary
  b <- arbitrary
  return $ Two a b

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

-- 3.
data Three a b c =
  Three a
        b
        c
  deriving (Show, Eq)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  Three m n f <*> Three m' n' x = Three (m <> m') (n <> n') (f x)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = genThree

genThree :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Three a b c)
genThree = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return $ Three a b c

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

-- 4.
data Three' a b =
  Three' a
         b
         b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

instance (Monoid a) => Applicative (Three' a) where
  pure x = Three' mempty x x
  Three' m f f' <*> Three' m' x y = Three' (m <> m') (f x) (f' y)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = genThree'

genThree' :: (Arbitrary a, Arbitrary b) => Gen (Three' a b)
genThree' = do
  a <- arbitrary
  b <- arbitrary
  return $ Three' a b b

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

-- 5.
data Four a b c d =
  Four a
       b
       c
       d
  deriving (Show, Eq)

instance Functor (Four a b c) where
  fmap f (Four j k l m) = Four j k l (f m)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure x = Four mempty mempty mempty x
  Four j k l m <*> Four j' k' l' m' = Four (j <> j') (k <> k') (l <> l') (m m')

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
         Arbitrary (Four a b c d) where
  arbitrary = genFour

genFour ::
     (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Gen (Four a b c d)
genFour = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  d <- arbitrary
  return $ Four a b c d

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

-- 6.
data Four' a b =
  Four' a
        a
        a
        b
  deriving (Show, Eq)

instance Functor (Four' a) where
  fmap f (Four' j k l m) = Four' j k l (f m)

instance (Monoid a) => Applicative (Four' a) where
  pure x = Four' mempty mempty mempty x
  Four' j k l m <*> Four' j' k' l' m' =
    Four' (j <> j') (k <> k') (l <> l') (m m')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = genFour'

genFour' :: (Arbitrary a, Arbitrary b) => Gen (Four' a b)
genFour' = do
  a <- arbitrary
  b <- arbitrary
  return $ Four' a a a b

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

-- Combinations
stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = undefined

spec :: Spec
spec =
  describe "Chapter Exercises" $ do
    it "works with them" $ do
      pendingWith "Takes too long to run"
      quickBatch (applicative (undefined :: Pair (Int, Int, Int)))
      quickBatch
        (applicative (undefined :: Two (String, String, String) (Int, Int, Int)))
      quickBatch
        (applicative
           (undefined :: Three (String, String, String) (String, String, String) ( Int
                                                                                 , Int
                                                                                 , Int)))
      quickBatch
        (applicative
           (undefined :: Three' (String, String, String) (Int, Int, Int)))
      quickBatch
        (applicative
           (undefined :: Four (String, String, String) (String, String, String) ( [Int]
                                                                                , [Int]
                                                                                , [Int]) ( Int
                                                                                         , Int
                                                                                         , Int)))
      quickBatch
        (applicative
           (undefined :: Four' (String, String, String) (Int, Int, Int)))
    it "finds the combination of 3 lists" $ do
      length (liftA3 (,,) stops vowels stops) `shouldBe` 180
