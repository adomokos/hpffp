module Hpffp.ParsingPhoneNumbersSpec where

import Test.Hspec
import Text.Trifecta

main :: IO ()
main = hspec spec

type NumberingPlanArea = Int -- (aka area code)
type Exchange = Int
type LineNumber = Int

data PhoneNumber =
    PhoneNumber NumberingPlanArea Exchange LineNumber
    deriving (Eq, Show)

parsePhone :: Parser PhoneNumber
parsePhone = do
    _ <- optional (string "1-")
    _ <- optional (char '(')
    npa <- count 3 digit
    _ <- optional (char ')')
    _ <- optional (oneOf " -")
    exc <- count 3 digit
    _ <- optional (oneOf " -")
    ln <- count 4 digit
    eof
    return $ PhoneNumber (read npa) (read exc) (read ln)

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

spec :: Spec
spec = do
    describe "Parsing Exercises" $ do
        it "parses phone numbers" $ do
            let result = maybeSuccess (parseString parsePhone
                                                   mempty
                                                   "123-832-4392")
            result `shouldBe` Just (PhoneNumber 123 832 4392)
            let result = maybeSuccess (parseString parsePhone
                                                   mempty
                                                   "123 832 4392")
            result `shouldBe` Just (PhoneNumber 123 832 4392)
            let result = maybeSuccess (parseString parsePhone
                                                   mempty
                                                   "1-123-832-4392")
            result `shouldBe` Just (PhoneNumber 123 832 4392)
