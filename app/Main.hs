module Main where

import Hello
import DogsRule
import System.IO
import Hpffp.AlgebraicDatatypes
import Hpffp.SignalingAdversity

main :: IO ()
main = runBuildPerson

runBuildPerson = do
    hSetBuffering stdout NoBuffering
    putStr "Please provide the name: "
    name <- getLine
    putStrLn "Please provide the age: "
    age <- getLine
    let parsedAge = (read age) :: Integer
        person = mkPerson' name parsedAge
    case person of
      Right person -> putStrLn $ "Yay! Successfully got a person: " ++ show person
      Left validationErrors -> putStrLn $ "Sorry, person did not get built: " ++ show validationErrors

runSayHello :: IO ()
runSayHello = do
    hSetBuffering stdout NoBuffering
    putStr "Please input your name: "
    name <- getLine
    sayHello name
    dogs

