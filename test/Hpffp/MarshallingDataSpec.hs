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

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Marshalling Data" $ do
        it "can deserialize string" $ do
            let color =
                    decode "{\"blue\": \"123\"}" :: Maybe Color
            color `shouldBe` Just (Blue "123")

            let anotherColor =
                    decode [r|{"red": "123"}|] :: Maybe Color
            anotherColor `shouldBe` Just (Red "123")

            let (Just d) =
                    decode sectionJson :: Maybe TestData
            section d `shouldBe` Host "wikipedia.org"
