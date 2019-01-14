module CH18.MaybeMonadSpec where

import Test.Hspec

main :: IO ()
main = hspec spec

data Cow = Cow
  { name :: String
  , age :: Int
  , weight :: Int
  } deriving (Show, Eq)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n | n >= 0 = Just n
             | otherwise = Nothing

weightCheck :: Cow -> Maybe Cow
weightCheck c =
  let w = weight c
      n = name c
  in if n == "Bess" && w > 499
        then Nothing
        else Just c

mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow name' age' weight' = do
  n <- noEmpty name'
  a <- noNegative age'
  w <- noNegative weight'
  weightCheck (Cow n a w)

spec :: Spec
spec =
  describe "Maybe Monad examples" $ do
    it "works with it, as Maybe is a Monad" $ do
      let cow = mkSphericalCow "Bess" 5 499
      cow `shouldBe` Just (Cow "Bess" 5 499)
      let cow' = mkSphericalCow "Bess" 5 500
      cow' `shouldBe` Nothing

