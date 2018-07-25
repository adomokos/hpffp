module Hpffp.MonadSpec where

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Monads" $ do
        it "fmap and applicatives can be combined to Monads" $ do
            let a = fmap (+1) [1..3]
                b = [1..3] >>= return . (+1)
            a `shouldBe` b
