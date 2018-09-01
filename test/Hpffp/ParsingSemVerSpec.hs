{-# LANGUAGE OverloadedStrings #-}
module Hpffp.ParsingSemVerSpec where

import Test.Hspec
import Data.Monoid ((<>))
import Control.Applicative
import Text.Trifecta

-- Relevant to precedence/ordering,
-- cannot sort numbers like strings.

data NumberOrString =
    NOSS String
  | NOSI Integer
    deriving (Show, Eq)

type Major = Integer
type Minor = Integer
type Patch = Integer
newtype Release = Release [NumberOrString] deriving (Show, Eq)
type Metadata = [NumberOrString]

data SemVer = SemVer Major Minor Patch Release Metadata
    deriving (Show, Eq)

ver :: String
ver = "1.0.0-x.7.z.92"

parseNOS :: Parser NumberOrString
parseNOS = (NOSI <$> try (decimal <* notFollowedBy letter))
           <|> NOSS <$> some (letter <|> digit)

parsePrerelease :: Parser NumberOrString
parsePrerelease = skipMany (oneOf ".") >> parseNOS

parseSemVer :: Parser SemVer
parseSemVer = SemVer
            <$> decimal
            <*> (char '.' *> decimal)
            <*> (char '.' *> decimal)
            <*> (Release <$> (char '-' *> some parsePrerelease <|> mempty))
            <*> (char '+' *> some parsePrerelease <|> mempty)

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

instance Ord NumberOrString where
    (NOSI _) `compare` (NOSS _) = GT
    (NOSS _) `compare` (NOSI _) = LT
    (NOSI x) `compare` (NOSI x') = x `compare` x'
    (NOSS x) `compare` (NOSS x') = x `compare` x'

instance Ord Release where
    (Release []) `compare` (Release []) = EQ
    (Release []) `compare` (Release _) = GT
    (Release _) `compare` (Release []) = LT
    (Release x) `compare` (Release x') = x `compare` x'

instance Ord SemVer where
    (SemVer mj mn p r _) `compare` (SemVer mj' mn' p' r' _) =
        compare mj mj'
            <> compare mn mn'
            <> compare p p'
            <> compare r r'



main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Parsing Exercises" $ do
        it "parses semantic versioning" $ do
            let result = maybeSuccess (parseString parseSemVer mempty "1.1.0")
            result `shouldBe` Just (SemVer 1 1 0 (Release []) [])
            let result = maybeSuccess (parseString parseSemVer mempty ver)
            result `shouldBe` Just (SemVer 1 0 0 (Release [NOSS "x",NOSI 7,NOSS "z",NOSI 92]) [])
            let result = maybeSuccess (parseString parseSemVer mempty "abc")
            result `shouldBe` Nothing
        it "can compare versions" $ do
            let ver1 = maybeSuccess (parseString parseSemVer mempty ver)
                ver2 = maybeSuccess (parseString parseSemVer mempty ver)
            ver1 `compare` ver2 `shouldBe` EQ
            let verA = maybeSuccess (parseString parseSemVer mempty "1.1.0")
                verB = maybeSuccess (parseString parseSemVer mempty "1.0.0")
            verA `compare` verB `shouldBe` GT
