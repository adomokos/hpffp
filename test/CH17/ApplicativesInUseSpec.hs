module CH17.ApplicativesInUseSpec where

import           Test.Hspec
import           Control.Applicative
import           Data.Char
import qualified Data.Map as M
import           Data.Monoid ( Sum(..), (<>) )
import           Data.List ( elemIndex )

main :: IO ()
main = hspec spec

f :: Int -> Maybe String
f x = lookup x [ (3, "hello")
               , (4, "julie")
               , (5, "kbai")]

g :: Int -> Maybe String
g y = lookup y [ (7, "sup?")
               , (8, "chris")
               , (9, "aloha")]

h :: Int -> Maybe Int
h z = lookup z [(2,3),(5,6),(7,8)]

m :: Int -> Maybe Int
m x = lookup x [(4,10),(8,13),(1,9001)]

getLine1 :: IO String
getLine1 = pure "hello"

getLine2 :: IO String
getLine2 = pure "there"

-- Lookup exercises

-- 1.
added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1,2,3] [4,5,6])

-- 2.
y :: Maybe Integer
y = lookup 3 $ zip [1,2,3] [4,5,6]

z :: Maybe Integer
z = lookup 2 $ zip [1,2,3] [4,5,6]

tupled :: Maybe (Integer,Integer)
tupled = (,) <$> y <*> z

-- 3.
x' :: Maybe Int
x' = elemIndex 3 [1,2,3,4,5]

y' :: Maybe Int
y' = elemIndex 4 [1,2,3,4,5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x' <*> y'

-- 4.
xs :: [Integer]
xs = [1,2,3]
ys :: [Integer]
ys = [4,5,6]

n :: Maybe Integer
n = lookup 3 $ zip xs ys

o :: Maybe Integer
o = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = sum <$> ((,) <$> n <*> o)

-- Exercise: Identity Instance
newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity x = Identity (f x)

-- Exercise: Constant Applicative
newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Show, Ord)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  Constant x <*> Constant y = Constant (x <> y)

spec :: Spec
spec = do
  describe "Applicatives In Use" $ do
    it "works with Lists as Functors" $ do
      fmap (2^) [1,2,3] `shouldBe` [2,4,8]
      fmap (^2) [1,2,3] `shouldBe` [1,4,9]
    it "works with Lists as Applicatives" $ do
      [(+1),(*2)] <*> [2,4] `shouldBe` [3,5,4,8]
      (,) <$> [1,2] <*> [3,4] `shouldBe` [(1,3),(1,4),(2,3),(2,4)]
      -- liftA2 is the same
      liftA2 (,) [1,2] [3,4] `shouldBe` [(1,3),(1,4),(2,3),(2,4)]
      (+) <$> [1,2] <*> [3,5] `shouldBe` [4,6,5,7]
      liftA2 (+) [1,2] [3,5] `shouldBe` [4,6,5,7]
      max <$> [1,2] <*> [1,4] `shouldBe` [1,4,2,4]
      liftA2 max [1,2] [1,4] `shouldBe` [1,4,2,4]
    it "works with lookup" $ do
      let l = lookup 3 [(3,"hello")]
      l `shouldBe` Just "hello"
      fmap length l `shouldBe` Just 5
      let y = lookup 4 [(3,"hello")]
      y `shouldBe` Nothing
      fmap length y `shouldBe` Nothing
      let c (x:xs) = toUpper x:xs
      fmap c l `shouldBe` Just "Hello"
    it "works with Data.Map" $ do
      let m = M.fromList [(3,"hello")]
          c (x:xs) = toUpper x:xs
      fmap c (M.lookup 3 m) `shouldBe` Just "Hello"
    it "works with more Applicativates" $ do
      f 3 `shouldBe` Just "hello"
      g 8 `shouldBe` Just "chris"
      (++) <$> f 3 <*> g 7 `shouldBe` Just "hellosup?"
      (+) <$> h 5 <*> m 1 `shouldBe` Just 9007
      (+) <$> h 5 <*> m 6 `shouldBe` Nothing
      liftA2 (++) (g 9) (f 4) `shouldBe` Just "alohajulie"
      liftA2 (^) (h 5) (m 4) `shouldBe` Just 60466176
      liftA2 (*) (h 5) (m 4) `shouldBe` Just 60
      liftA2 (*) (h 1) (m 1) `shouldBe` Nothing
    it "works with IO as well" $ do
      result <- (++) <$> getLine1 <*> getLine2
      result `shouldBe` "hellothere"
      result2 <- (,) <$> getLine1 <*> getLine2
      result2 `shouldBe` ("hello","there")
      result3 <- fmap length ((++) <$> getLine1 <*> getLine2)
      result3 `shouldBe` 10
  describe "Exercise Lookups" $ do
    it "type checks, 1." $ do
      added `shouldBe` Just 9
    it "type checks, 2." $ do
      tupled `shouldBe` Just (6,5)
    it "type checks, 3." $ do
      maxed `shouldBe` Just 3
    it "type checks, 4." $ do
      summed `shouldBe` Just 5
  describe "Identity as Applicative" $
    it "works with lists" $ do
      let result  = const <$> [1,2,3] <*> [4,5,6]
          result' = const <$> Identity [1,2,3] <*> Identity [4,5,6]
      result `shouldBe` [1,1,1,2,2,2,3,3,3]
      result' `shouldBe` Identity [1,2,3]
  describe "Constant as Applicative" $
    it "works with Sum" $ do
      let f = Constant (Sum 1)
          g = Constant (Sum 2)
          result = f <*> g
      result `shouldBe` Constant (Sum 3)
      (pure 1 :: Constant String Int) `shouldBe` Constant ""
