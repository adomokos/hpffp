module Main where

import Hello
import DogsRule
import System.IO
import Hpffp.AlgebraicDatatypes

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStr "Please input your name: "
    name <- getLine
    sayHello name
    dogs
