{-# LANGUAGE RankNTypes #-}
module Hpffp.ParserIntroSpec where

import Test.Hspec
import Text.Trifecta
import Text.Parser.Combinators

main :: IO ()
main = hspec spec

stop :: Parser a
stop = unexpected "stop"

one :: Parser Char
one = char '1'

two :: Parser Char
two = char '2'

three :: Parser Char
three = char '3'

one' = one >> stop

-- read two characters, '1', and '2'
oneTwo = char '1' >> char '2'

-- read two characters, '1' and '2', then die
oneTwo' = oneTwo >> stop

type S = forall m. CharParsing m => m String

oneS :: S
oneS = string "1"

oneTwoS :: S
oneTwoS = string "12"

oneTwoThreeS :: S
oneTwoThreeS = string "123"

testParse :: Parser Char -> String
testParse p =
    show (parseString p mempty "123")

testEOF :: Parser () -> String
testEOF p =
    show (parseString p mempty "123")

spec :: Spec
spec = do
    describe "Parsing" $ do
        it "pulls parsed chars" $ do
            testParse one `shouldBe` "Success '1'"
            testParse oneTwo `shouldBe` "Success '2'"
        it "pulls more than one chars" $ do
            show (parseString oneS mempty "123") `shouldBe` "Success \"1\""
            show (parseString oneTwoS mempty "123") `shouldBe` "Success \"12\""
            show (parseString oneTwoThreeS mempty "123") `shouldBe` "Success \"123\""
        it "can compbine all" $ do
            let parsers = choice [ oneTwoThreeS
                                 , oneTwoS
                                 , oneS
                                 , stop]
            show (parseString parsers mempty "123") `shouldBe` "Success \"123\""
            let more_parsers = choice [ one >> two >> three
                                      , one >> two
                                      , one
                                      , stop ]
            show (parseString more_parsers mempty "123") `shouldBe` "Success '3'"
