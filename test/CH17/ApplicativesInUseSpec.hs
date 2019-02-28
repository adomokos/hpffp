module CH17.ApplicativesInUseSpec (spec) where

import           Test.Hspec
import           Control.Applicative
import           Data.Char
import qualified Data.Map as M
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
h z' = lookup z' [(2,3),(5,6),(7,8)]

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
theYs :: Maybe Integer
theYs = lookup 3 $ zip [1,2,3] [4,5,6]

z :: Maybe Integer
z = lookup 2 $ zip [1,2,3] [4,5,6]

tupled :: Maybe (Integer,Integer)
tupled = (,) <$> theYs <*> z

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
applicativeXs :: [Integer]
applicativeXs = [1,2,3]

applicativeYs :: [Integer]
applicativeYs = [4,5,6]

n :: Maybe Integer
n = lookup 3 $ zip applicativeXs applicativeYs

o :: Maybe Integer
o = lookup 2 $ zip applicativeXs applicativeYs

summed :: Maybe Integer
summed = sum <$> ((,) <$> n <*> o)

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
      let c' (x:xs) = toUpper x:xs
          c' [] = []
      fmap c' l `shouldBe` Just "Hello"
    it "works with Data.Map" $ do
      let m' = M.fromList [(3,"hello")]
          c' (x:xs) = toUpper x:xs
          c' [] = []
      fmap c' (M.lookup 3 m') `shouldBe` Just "Hello"
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
