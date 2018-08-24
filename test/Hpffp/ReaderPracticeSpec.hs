module Hpffp.ReaderPracticeSpec where

import Test.Hspec
import Control.Applicative
import Data.Maybe

main :: IO ()
main = hspec spec

x = [1,2,3]
y = [4,5,6]
z = [7,8,9]

{- lookup :: Eq a => a -> [(a, b)] -> Maybe b -}
{- lookup = undefined -}

xs :: Maybe Integer
xs = lookup 3 $ zip x y

spec :: Spec
spec = do
    describe "Reader Practices" $ do
        it "can simulate DB calls" $ do
            xs `shouldBe` Just 6

