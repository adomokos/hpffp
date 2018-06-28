{-# LANGUAGE DeriveGeneric #-}

module Hpffp.CoArbitrarySpec where

import Test.Hspec
import Test.QuickCheck
import GHC.Generics

data Bool' =
    True'
  | False'
    deriving (Generic)

instance CoArbitrary Bool'

trueGen :: Gen Int
trueGen = coarbitrary True' arbitrary

falseGen :: Gen Int
falseGen = coarbitrary False' arbitrary

spec :: Spec
spec = do
    describe "CoArbitrary" $ do
        it "can generate with a function" $ do
            pending
