module Hpffp.ApplicativeExercisesSpec where

import Control.Applicative
import Data.Monoid
import Test.Hspec
import Test.Hspec.Checkers
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

main :: IO ()
main = hspec spec

{-
data Bull
    = Fools
    | Twoo
    deriving (Eq, Show)

instance Arbitrary Bull where
    arbitrary = frequency [(1, return Fools), (1, return Twoo)]

instance Monoid Bull where
    mempty = Fools
    mappend _ _ = Fools

instance EqProp Bull where
    (=-=) = eq
-}

-- ZipList Examples

-- this isn't going to work properly
instance Monoid a => Monoid (ZipList a) where
    mempty = ZipList []
    mappend = liftA2 mappend

{- instance Arbitrary a -}
    {- => Arbitrary (ZipList a) where -}
    {- arbitrary = ZipList <$> arbitrary -}

{- instance Arbitrary a -}
    {- => Arbitrary (Sum a) where -}
    {- arbitrary = Sum <$> arbitrary -}

instance Eq a => EqProp (ZipList a) where
    (=-=) = eq

data List a =
    Nil
  | Cons a (List a)
  deriving (Show, Eq)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Monoid (List a) where
    mempty = Nil
    mappend = append

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ fmap f as

instance Applicative List where
    pure x = Cons x Nil
    _ <*> Nil = Nil
    Nil <*> _ = Nil
    (Cons f fs) <*> xs = append (fmap f xs) (fs <*> xs)

instance Eq a => EqProp (List a) where
    xs =-= ys = xs' `eq` ys
        where xs' = let l = xs
                    in take' 3000 l
              ys' = let l = ys
                    in take' 3000 l

take' :: Int -> List a -> List a
take' n xs = f n xs Nil
    where f n' (Cons h t) acc =
            if n' == 0
            then acc
            else f (n' - 1) t (Cons h acc)
          f _ Nil acc = acc

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = genList


genList :: Arbitrary a => Gen (List a)
genList = do
    h <- arbitrary
    t <- genList
    frequency [(3, return $ Cons h t),
               (1, return Nil)]

spec :: Spec
spec = do
    describe "ZipList Monoid" $ do
        it "can concat lists" $
            [1,2,3] <> [4,5,6]
                `shouldBe` [1..6]
        it "can concat Sum newtype values" $ do
            let result = 1 <> (2 :: Data.Monoid.Sum Integer)
            getSum result `shouldBe` 3
        it "works for custom List" $ do
            let f = Cons (+1) (Cons (*2) Nil)
            let v = Cons  1 (Cons 2 Nil)
            f <*> v `shouldBe` Cons 2 (Cons 3 (Cons 2 (Cons 4 Nil)))
        it "flattens after fmap for flatmap" $ do
            let result = fmap (\x -> [x, 9]) [1,2,3]
            result `shouldBe` [[1,9],[2,9],[3,9]]
        testBatch (monoid (undefined :: [Int]))
