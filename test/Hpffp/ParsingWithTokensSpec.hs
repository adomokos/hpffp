{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Hpffp.ParsingWithTokensSpec where

import Test.Hspec
import Text.Trifecta

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

main :: IO ()
main = hspec spec

p' :: Parser [Integer]
p' = some $ do
    i <- token (some digit)
    return (read i)

spec :: Spec
spec = do
    describe "Parsing with Tokens" $ do
        it "parses the values from String" $ do
            maybeSuccess(parseString (some digit) mempty "123 456")
                `shouldBe` Just "123"
            maybeSuccess(parseString (some (some digit)) mempty "123 456")
                `shouldBe` Just ["123"]
            maybeSuccess(parseString (some integer) mempty "123456")
                `shouldBe` Just [123456]
            maybeSuccess(parseString (some integer) mempty "123 456")
                `shouldBe` Just [123,456]
            maybeSuccess(parseString (some integer) mempty "123 \n \n 456")
                `shouldBe` Just [123,456]
        it "works with Tokens, but not how you expect it" $ do
            let s = "123 \n  \n 456"
            maybeSuccess(parseString (token (some digit)) mempty s)
                `shouldBe` Just "123"
            maybeSuccess(parseString (token (some (token digit))) mempty s)
                `shouldBe` Just "123456"
            maybeSuccess(parseString (some decimal) mempty s)
                `shouldBe` Just [123]
            maybeSuccess(parseString (some (token decimal)) mempty s)
                `shouldBe` Just [123,456]
        it "can parse integers with 'integer', as it's a tokenizer" $ do
            maybeSuccess(parseString (some integer) mempty "1\n2\n 3\n")
                `shouldBe` Just [1,2,3]
        it "can use a tokenizing parser" $ do
            let s = "1\n2\n3"
            maybeSuccess(parseString p' mempty s)
                `shouldBe` Just [1,2,3]
            maybeSuccess(parseString (token (some digit)) mempty s)
                `shouldBe` Just "1"
            maybeSuccess(parseString (some (token (some digit))) mempty s)
                `shouldBe` Just ["1","2","3"]
        it "can work with two char tokenizer" $ do
            let tknWhole = token $ char 'a' >> char 'b'
            maybeSuccess(parseString tknWhole mempty "a b")
                `shouldBe` Nothing -- Failure
            maybeSuccess(parseString (some tknWhole) mempty "ab ab")
                `shouldBe` Just "bb"
            let tknCharA = (token (char 'a')) >> char 'b'
            maybeSuccess(parseString tknCharA mempty "a b")
                `shouldBe` Just 'b'
            maybeSuccess(parseString (some tknCharA) mempty "a ba b")
                `shouldBe` Just "bb"
            maybeSuccess(parseString (some tknCharA) mempty "a b a b")
                `shouldBe` Just "b"
            let tknBoth = token (char 'a') >> token (char 'b')
            maybeSuccess(parseString (some tknBoth) mempty "a b a b")
                `shouldBe` Just "bb"


