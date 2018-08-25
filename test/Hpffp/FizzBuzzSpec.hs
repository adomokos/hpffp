module Hpffp.FizzBuzzSpec where

import Test.Hspec
import Control.Monad
import Control.Monad.Trans.State
import qualified Data.DList as DL

main :: IO ()
main = hspec spec

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5 == 0 = "Buzz"
           | n `mod` 3 == 0 = "Fizz"
           | otherwise      = show n

fizzBuzzList :: [Integer] -> [String]
fizzBuzzList list =
    execState (mapM_ addResult list) []

addResult :: Integer -> State [String] ()
addResult n = do
    xs <- get
    let result = fizzBuzz n
    -- snoc appends to the end, unlike
    -- cons, which adds to the front
    put (result : xs)

fizzBuzzList' :: [Integer] -> [String]
fizzBuzzList' list =
    let dlist =
         execState (mapM_ addResult' list) DL.empty
         -- convert back to normal list
    in DL.apply dlist []

addResult' :: Integer -> State (DL.DList String) ()
addResult' n = do
    xs <- get
    let result = fizzBuzz n
    -- snoc appends to the end, unlike
    -- cons, which adds to the front
    put (DL.snoc xs result)

fizzBuzzFromTo :: Integer -> Integer -> [String]
fizzBuzzFromTo x y
    | x == y = fizzBuzzList [x]
    | x < y && y - x == 1 = fizzBuzzList [y,x]
    | x < y = fizzBuzzList [y, y - 1 .. x]
    | otherwise = fizzBuzzFromTo y x

spec :: Spec
spec = do
    describe "FizzBuzz" $ do
        it "works with the trivial implementation" $ do
            take 5 (map fizzBuzz [1..5])
                `shouldBe` ["1","2","Fizz","4","Buzz"]
        it "works with State" $ do
            take 5 (fizzBuzzList [1..100])
                `shouldBe` ["Buzz","Fizz","98","97","Fizz"]
            (take 5 . reverse) (fizzBuzzList [1..100])
                `shouldBe` ["1","2","Fizz","4","Buzz"]
            (take 5) (fizzBuzzList' [1..100])
                `shouldBe` ["1","2","Fizz","4","Buzz"]
        it "works for fizzBuzzFromTo" $ do
            fizzBuzzFromTo 12 16 `shouldBe`
                ["Fizz","13","14","FizzBuzz","16"]
