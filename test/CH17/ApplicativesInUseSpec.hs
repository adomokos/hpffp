module CH17.ApplicativesInUseSpec where

import           Test.Hspec
import           Control.Applicative
import           Data.Char
import qualified Data.Map as M

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Applicatives In Use" $ do
    it "works with Lists as Functors" $ do
      fmap (2^) [1,2,3] `shouldBe` [2,4,8]
      fmap (^2) [1,2,3] `shouldBe` [1,4,9]
    it "works with Lists as Applicatives" $ do
      [(+1),(*2)] <*> [2,4] `shouldBe` [3,5,4,8]
      (,) <$> [1,2] <*> [3,4] `shouldBe` [(1,3),(1,4),(2,3),(2,4)]
      -- liftA2 is the same
      liftA2 (,) [1,2] [3,4] `shouldBe` [(1,3),(1,4),(2,3),(2,4)]
      (+) <$> [1,2] <*> [3,5] `shouldBe` [4,6,5,7]
      liftA2 (+) [1,2] [3,5] `shouldBe` [4,6,5,7]
      max <$> [1,2] <*> [1,4] `shouldBe` [1,4,2,4]
      liftA2 max [1,2] [1,4] `shouldBe` [1,4,2,4]
    it "works with lookup" $ do
      let l = lookup 3 [(3,"hello")]
      l `shouldBe` Just "hello"
      fmap length l `shouldBe` Just 5
      let y = lookup 4 [(3,"hello")]
      y `shouldBe` Nothing
      fmap length y `shouldBe` Nothing
      let c (x:xs) = toUpper x:xs
      fmap c l `shouldBe` Just "Hello"
    it "works with Data.Map" $ do
      let m = M.fromList [(3,"hello")]
          c (x:xs) = toUpper x:xs
      fmap c (M.lookup 3 m) `shouldBe` Just "Hello"
