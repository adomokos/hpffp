module Hpffp.ApplicativeValidationSpec where

import Test.Hspec
import Control.Applicative (liftA3)

main :: IO ()
main = hspec spec

validateLength :: Int -> String -> Maybe String
validateLength maxLen s =
    if (length s) > maxLen
    then Nothing
    else Just s

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = fmap Name $ validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress a = fmap Address $ validateLength 100 a

data Person = Person Name Address deriving (Eq, Show)

mkPerson :: String -> String -> Maybe Person
mkPerson n a =
    case mkName n of
      Nothing -> Nothing
      Just n' ->
          case mkAddress a of
            Nothing -> Nothing
            Just a' -> Just $ Person n' a'

-- Before we moooove on

data Cow = Cow {
      name :: String
    , age :: Int
    , weight :: Int
    } deriving (Show, Eq)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n
  | n >= 0 = Just n
  | otherwise = Nothing

-- Validating to get rid of empty
-- strings, negative numbers
cowFromString :: String
              -> Int
              -> Int
              -> Maybe Cow
cowFromString name' age' weight' =
    case noEmpty name' of
      Nothing -> Nothing
      Just nammy ->
          case noNegative age' of
            Nothing -> Nothing
            Just agey ->
                case noNegative weight' of
                  Nothing -> Nothing
                  Just weighty -> Just (Cow nammy agey weighty)

-- Shorter, more compact version with Applicative
cowFromString' :: String -> Int -> Int -> Maybe Cow
cowFromString' name' age' weight' =
    Cow <$> noEmpty name'
        <*> noNegative age'
        <*> noNegative weight'

-- Or if want other Haskellers to think we're
-- really cool and hip
cowFromString'' :: String -> Int -> Int -> Maybe Cow
cowFromString'' name' age' weight' =
    liftA3 Cow (noEmpty name')
               (noNegative age')
               (noNegative weight')


spec :: Spec
spec = do
    describe "Validation" $ do
        it "validates person" $ do
            mkPerson "John" "Somewhere in Chicago"
                `shouldBe` Just (Person (Name "John")
                                        (Address "Somewhere in Chicago"))
            mkPerson (map (\x -> 'x') [1..30]) "Address"
                `shouldBe` Nothing
        it "can use Applicatives" $ do
            Person <$> mkName "Babe" <*> mkAddress "Some address"
                `shouldBe` Just (Person (Name "Babe") (Address "Some address"))
        it "can create a Cow" $ do
            cowFromString "Moo" 4 500 `shouldBe`
                Just (Cow "Moo" 4 500)
            cowFromString "" 4 500 `shouldBe` Nothing
            cowFromString' "Moo" 4 500 `shouldBe`
                Just (Cow "Moo" 4 500)
            cowFromString' "" 4 500 `shouldBe` Nothing
