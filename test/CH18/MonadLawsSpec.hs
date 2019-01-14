module CH18.MonadLawsSpec where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

main :: IO ()
main = hspec spec

{-
   1. Identity

   -- right identity
   m >>= return = m

   -- left identity
   return x >>= f = f x

  2. Associativity

  (m >>= f) >>= g = m >>= (\x -> f x >>= g)

  Regrouping the functions should not have any impact on the
  final result.
-}

spec :: Spec
spec =
  describe "Monad Laws" $ do
    let listTrigger :: [(Int, String, Int)]
        listTrigger = undefined
        maybeTrigger :: Maybe (Int, String, Int)
        maybeTrigger = undefined
    it "can be verified with quickBatch" $ do
      pending
      quickBatch (functor listTrigger)
      quickBatch (applicative listTrigger)
      quickBatch (monad listTrigger)
      quickBatch (functor maybeTrigger)
      quickBatch (applicative maybeTrigger)
      quickBatch (monad maybeTrigger)
