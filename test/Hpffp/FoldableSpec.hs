module Hpffp.FoldableSpec where

import Test.Hspec
import Data.Foldable
import Data.Monoid

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Foldable" $ do
        it "can fold with Monoid types" $ do
            foldr (+) 0 [1..5] `shouldBe` 15
            let xs = map Sum [1..5]
            getSum (fold xs) `shouldBe` 15
            let ys = map Product [1..5]
            getProduct (fold ys) `shouldBe` 120
            foldr (++) "" ["hello", " julie"]
                `shouldBe` "hello julie"
            fold ["hello", " julie"]
                `shouldBe` "hello julie"
            -- The default Monoid instance for lists gives us what
            -- we need without having to specify it
        describe "foldMap" $ do
            it "has a function as first argument in foldMap" $ do
                let sum = foldMap Sum [1..4]
                getSum sum `shouldBe` 10
                let product = foldMap Product [1..4]
                getProduct product `shouldBe` 24
                let all = foldMap All [True, False, True]
                getAll all `shouldBe` False
                let any = foldMap Any [(3 == 4), (9 > 5)]
                getAny any `shouldBe` True
            it "can have mapping function that's different from the Monoid" $ do
                let xs = map Product [1..3]
                    result = foldMap (*5) xs
                getProduct result `shouldBe` 750
                let ys = map Sum [1..3]
                    result = foldMap (*5) ys
                getSum result `shouldBe` 30
                -- for foldr, the funct has the Monoid instance baked in
                foldr (*) 5 [1..3] `shouldBe` 30
                -- however, the Monoid type is ignored
                let sumXs = map Sum [2..4]
                getSum(foldr (*) 3 sumXs) `shouldBe` 72
                let productXs = map Product [2..4]
                getProduct (foldr (*) 3 productXs) `shouldBe` 72
                -- Monoid type is ignored with one instance only
                let fm = foldMap (*5)
                getProduct (fm (Just 100) :: Product Integer)
                    `shouldBe` 500
                let gm = foldMap (*5)
                getSum (gm (Just 5) :: Sum Integer) `shouldBe` 25
                getSum (gm Nothing :: Sum Integer) `shouldBe` 0
                getProduct (fm Nothing :: Product Integer) `shouldBe` 1
