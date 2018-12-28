module CH17.ApplicativeLawsSpec where

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Applicative Laws" $ do
    it "conforms to Identity" $ do
      pure id <*> [1..5] `shouldBe` [1..5]
      pure id <*> Just "Hello" `shouldBe` Just "Hello"
      pure id <*> (Nothing :: Maybe Int) `shouldBe` Nothing
      pure id <*> (Left "Error'ish" :: Either String Int)
        `shouldBe` Left "Error'ish"
      pure id <*> (Right 8001 :: Either String Int)
        `shouldBe` Right 8001
      (pure id <*> (+1)) 2 `shouldBe` 3
      fmap id [1..5] `shouldBe` [1..5]
    it "conforms to Composition" $ do
      pure (.) <*> [(+1)]
               <*> [(*2)]
               <*> [1,2,3]
        `shouldBe` [3,5,7]
      -- Same as
      [(+1)] <*> ([(*2)] <*> [1,2,3])
        `shouldBe` [3,5,7]
      pure (.) <*> Just (+1)
               <*> Just (*2)
               <*> Just 1
        `shouldBe` Just 3
      -- Same as
      Just (+1) <*> (Just (*2) <*> Just 1)
        `shouldBe` Just 3
    it "conforms to Homomorphism" $ do
      (pure (+1) <*> pure 1 :: Maybe Int) `shouldBe` Just 2
      -- Same as
      (pure ((+1) 1) :: Maybe Int) `shouldBe` Just 2
      -- More things to try
      (pure (+1) <*> pure 1 :: [Int]) `shouldBe` [2]
      (pure (+1) <*> pure 1 :: Either String Int) `shouldBe` Right 2
    it "conforms to Interchange" $ do
      Just (+2) <*> pure 2 `shouldBe` Just 4
      pure ($ 2) <*> Just (+2) `shouldBe` Just 4
