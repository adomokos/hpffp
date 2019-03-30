module CH17.ApplicativeCheckersSpec where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Bull =
    Fools
  | Twoo
  deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary =
    frequency [ (1, return Fools)
              , (1, return Twoo)]

instance Semigroup Bull where
  _ <> _ = Fools

instance Monoid Bull where
  mempty = Fools

instance EqProp Bull where (=-=) = eq

main :: IO ()
main = hspec spec

type SSI = (String, String, Int)

trigger :: [SSI]
trigger = undefined

data List a =
    Nil
  | Cons a (List a)
  deriving (Show, Eq)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x list) = Cons (f x) (fmap f list)

instance Semigroup (List a) where
  (<>) = append

instance Monoid (List a) where
  mempty = Nil

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
          _ys' = let l = ys
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

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
      where xs' = let (ZipList' l) = xs
                  in take' 3000 l
            ys' = let (ZipList' l) = ys
                  in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure x = ZipList' (Cons x Nil)
  _ <*> ZipList' Nil = ZipList' Nil
  ZipList' Nil <*> _ = ZipList' Nil
  ZipList' (Cons f Nil) <*> ZipList' (Cons x xs) =
    ZipList' $ Cons (f x) (pure f <*> xs)
  ZipList' (Cons f fs) <*> ZipList' (Cons x Nil) =
    ZipList' $ Cons (f x) (fs <*> pure x)
  ZipList' (Cons f fs) <*> ZipList' (Cons x xs) =
    ZipList' $ Cons (f x) (fs <*> xs)

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = genZipList

genZipList :: Arbitrary a => Gen (ZipList' a)
genZipList = do
  l <- arbitrary
  return $ ZipList' l

spec :: Spec
spec =
  describe "Running Checkers" $ do
    it "verifies it" $ do
      pendingWith "No need to run it"
      -- quickBatch (monoid Twoo)
      let xs = ([("b", "w", 1)] :: [([Char],[Char],Int)])
      quickBatch $ applicative xs
      quickBatch $ applicative trigger
      -- let list' = [((Cons 2 (Cons 3 Nil)), Nil, Nil)] :: [(List Int, List Int, List String)]
      -- quickBatch $ applicative list'
      quickBatch (monoid (undefined :: [Int]))
      quickBatch (applicative $ ZipList' (Cons (undefined :: (Bool, Bool, Bool))Nil))
