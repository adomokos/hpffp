{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Hpffp.MarshallingDataSpec where

import Test.Hspec
import Control.Applicative
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Data.Text (Text)
import Text.RawString.QQ
import Data.Scientific (floatingOrInteger)

sectionJson :: LBS.ByteString
sectionJson = [r|
{ "section": {"host": "wikipedia.org"},
  "whatisit": {"red": "intoothandclaw"}
}
|]

data TestData =
    TestData {
      section :: Host
    , what :: Color
    } deriving (Eq, Show)

newtype Host = Host String deriving (Eq, Show)

type Annotation = String

data Color =
    Red Annotation
  | Blue Annotation
  | Yellow Annotation
  deriving (Eq, Show)

instance FromJSON TestData where
    parseJSON (Object v) =
        TestData <$> v .: "section"
                 <*> v .: "whatisit"
    parseJSON _ =
        fail "Expected an object for TestData"

instance FromJSON Host where
    parseJSON (Object v) =
        Host <$> v .: "host"
    parseJSON _ =
        fail "Expected an object for Host"

instance FromJSON Color where
    parseJSON (Object v) =
            (Red <$> v .: "red")
        <|> (Blue <$> v .: "blue")
        <|> (Yellow <$> v .: "yellow")
    parseJSON _ =
        fail "Expected an object for Color"

data NumberOrString =
    Numba Integer
  | Stringy Text
  deriving (Eq, Show)

instance FromJSON NumberOrString where
    parseJSON (Number i) =
        case floatingOrInteger i of
            (Left _) ->
                fail "Must be integral number"
            (Right integer) ->
                return $ Numba integer
    parseJSON (String s) = return $ Stringy s
    parseJSON _ =
        fail "NumberOrString must be number or string"

dec :: LBS.ByteString -> Maybe NumberOrString
dec = decode

eitherDec :: LBS.ByteString -> Either String NumberOrString
eitherDec = eitherDecode

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Marshalling Data" $ do
        it "can deserialize from string" $ do
            let color =
                    decode "{\"blue\": \"123\"}" :: Maybe Color
            color `shouldBe` Just (Blue "123")

            let anotherColor =
                    decode [r|{"red": "123"}|] :: Maybe Color
            anotherColor `shouldBe` Just (Red "123")

            let (Just d) =
                    decode sectionJson :: Maybe TestData
            section d `shouldBe` Host "wikipedia.org"
    describe "Number or String" $ do
        it "can deserialize from string" $ do
            dec "blah" `shouldBe` Nothing
            dec "\"blah\"" `shouldBe` Just (Stringy "blah")
            dec "123" `shouldBe` Just (Numba 123)
