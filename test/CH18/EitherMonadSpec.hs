module CH18.EitherMonadSpec where

import Test.Hspec

main :: IO ()
main = hspec spec

type Founded = Int
type Coders = Int

data SoftwareShop =
  Shop {
      founded :: Founded
    , programmers :: Coders
  } deriving (Show, Eq)

data FoundedError =
    NegativeYears Founded
  | TooManyYears Founded
  | NegativeCoders Coders
  | TooManyCoders Coders
  | TooManyCodersForYears Founded Coders
  deriving (Eq, Show)

validateFounded :: Int -> Either FoundedError Founded
validateFounded n
  | n < 0     = Left $ NegativeYears n
  | n > 500   = Left $ TooManyYears n
  | otherwise = Right n

validateCoders :: Int -> Either FoundedError Coders
validateCoders n
  | n < 0     = Left $ NegativeCoders n
  | n > 5000  = Left $ TooManyCoders n
  | otherwise = Right n

mkSoftware :: Int -> Int -> Either FoundedError SoftwareShop
mkSoftware years coders = do
  founded <- validateFounded years
  programmers <- validateCoders coders
  if programmers > div founded 10
     then Left $
            TooManyCodersForYears
            founded programmers
    else Right $ Shop founded  programmers

spec :: Spec
spec =
  describe "Either Monad" $ do
    it "works like a Monad should" $ do
      mkSoftware 0 0 `shouldBe` Right (Shop 0 0 )
      mkSoftware (-1) 0 `shouldBe` Left (NegativeYears (-1))
      mkSoftware (-1) (-1) `shouldBe` Left (NegativeYears (-1))
      mkSoftware (0) (-1) `shouldBe` Left (NegativeCoders (-1))
      mkSoftware 500 0 `shouldBe` Right (Shop 500 0)
      mkSoftware 501 0 `shouldBe` Left (TooManyYears 501)
      mkSoftware 501 501 `shouldBe` Left (TooManyYears 501)
      mkSoftware 100 5001 `shouldBe` Left (TooManyCoders 5001)
      mkSoftware 0 500 `shouldBe` Left (TooManyCodersForYears 0 500)
