module Hpffp.MonadLawsSpec where

import Test.Hspec
import Test.Hspec.Checkers
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

main :: IO ()
main = hspec spec

-- Test a Bad Monad first

data CountMe a =
    CountMe Integer a
    deriving (Eq, Show)

instance Functor CountMe where
    fmap f (CountMe i a) =
        CountMe i (f a)

instance Applicative CountMe where
    pure = CountMe 0
    CountMe n f <*> CountMe n' a =
        CountMe (n + n') (f a)

instance Monad CountMe where
    return = pure

    CountMe _ a >>= f = f a
    -- This was the original
    {- CountMe n a >>= f = -}
        {- let CountMe _ b = f a -}
         {- in CountMe (n + 1) b -}

instance Arbitrary a => Arbitrary (CountMe a) where
    arbitrary = CountMe <$> arbitrary <*> arbitrary

instance Eq a => EqProp (CountMe a) where
    (=-=) = eq

spec :: Spec
spec = do
    describe "Monad Laws with Checkers" $ do
        let subject :: [(Int,Int,Int)]
            subject = undefined
        testBatch $ monad subject
    describe "Incorrect Monad" $ do
        let trigger :: CountMe (Int, String, Int)
            trigger = undefined
        testBatch $ functor trigger
        testBatch $ applicative trigger
        -- This still does not pass
        {- testBatch $ monad trigger -}
