{-# LANGUAGE OverloadedStrings #-}
module Hpffp.ParsingFailureSpec where

import Test.Hspec
import Control.Applicative
import qualified Data.Attoparsec.ByteString as A
import Data.Attoparsec.ByteString (parseOnly)
import Data.ByteString (ByteString, unpack)
import Text.Trifecta hiding (parseTest)
import Text.Parsec (Parsec, ParseError, parse)

trifP :: Show a => Parser a -> String -> Result a
trifP p i = parseString p mempty i

attoP :: Show a
      => A.Parser a
      -> ByteString
      -> Either String a
attoP p i = parseOnly p i

parsecP :: (Show a)
        => Parsec String () a
        -> String
        -> String
        -> Either ParseError a
parsecP = parse

nobackParse :: (Monad f, CharParsing f)
            => f Char
nobackParse =
    (char '1' >> char '2') <|> char '3'

tryParse :: (Monad f, CharParsing f)
         => f Char
tryParse =
    try (char '1' >> char '2') <|> char '3'

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Parsing failure" $ do
        it "works with Trifecta" $ do
            -- trifecta
            let (Failure x) = trifP nobackParse "13"
            show x `shouldContain` "ErrInfo"
            let (Failure x) = trifP tryParse "13"
            {- parsecP nobackParse "13" -}
            show x `shouldContain` "ErrInfo"
            -- parsec
            let (Left err) = parsecP nobackParse "" "13"
            show err `shouldContain` "unexpected" -- error
            let (Left err) = parsecP tryParse "" "13"
            show err `shouldContain` "unexpected" -- error
            -- attoparsec
            let (Left result) = attoP nobackParse "13"
            result `shouldBe` "\"3\": satisfyElem"
            let (Left result) = attoP tryParse "13"
            result `shouldBe` "\"3\": satisfyElem"


