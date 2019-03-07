module CH21.StrengthForUnderstandingSpec where

import Test.Hspec

import Data.Functor.Identity
import Data.Monoid --from transformers
import Data.Functor.Constant

{-
  Traversable is stronger than Functor and Foldable. Because of this
  we can recover the Functor and Foldable instance for a type from the
  Traversable, just as we can recover Functor and Applicative from the
  Monad.
-}


main :: IO ()
main = hspec spec

edgeMap :: Traversable t => (a -> b) -> t a -> t b
edgeMap f t = runIdentity $ traverse (Identity . f) t

foldMap' :: (Traversable t, Monoid a) => (a1 -> a) -> t a1 -> a
foldMap' f t = getConstant $ traverse (Constant . f) t

spec :: Spec
spec = do
  describe "Strength for understanding" $ do
    it "works with Identity, just like fmap" $ do
      traverse (Identity . (+1)) [1,2]
        `shouldBe` Identity [2,3]
      runIdentity (traverse (Identity . (+1)) [1,2])
        `shouldBe` [2,3]
    it "works with constructed Function, edgeMap" $
      edgeMap (+1) [1..5] `shouldBe` [2..6]
  describe "using Const and Constant" $ do
    it "can use a foldMappy-looking foldable" $ do
      let xs' = [1,2,3,4,5]
          xs  = xs' :: [Sum Integer]
      traverse (Constant . (+1)) xs
        `shouldBe` Constant (Sum 20)
      foldMap' (+1) xs `shouldBe`Sum 20
