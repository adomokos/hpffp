{-# LANGUAGE FlexibleInstances #-}
module CH16.ChapterExercisesSpec where

import Test.Hspec

main :: IO ()
main = hspec spec

{-
  1. data Bool = False | True
  No Functor instance can be declared, as it's kind is `Bool :: *`
-}

-- 2.
data BoolAndSomethingElse a =
  False' a | True' a
  deriving (Show, Eq)

instance Functor (BoolAndSomethingElse) where
  fmap f (False' x) = False' (f x)
  fmap f (True' x) = True' (f x)

-- 3.
data BoolAndMaybeSomethingElse a =
  Falsish | Truish a
  deriving (Show, Eq)

instance Functor (BoolAndMaybeSomethingElse) where
  fmap _ Falsish = Falsish
  fmap f (Truish x) = Truish (f x)

-- 4.
{-
newtype Mu f = InF { outF :: f (Mu f) } -- deriving (Show, Eq)
-- Maybe it's doable, I can't figure it out, though
-}

-- 5.
{-
   import GHC.Arr

   data D = D (Array Word Word) Int Int
   -- Nope, not possible
-}

-- 1.
data Sum a b = First a | Second b deriving (Show, Eq)

instance Functor (Sum a) where
  fmap _ (First x) = First x
  fmap f (Second y) = Second (f y)

-- 2.
data Company a b c =
    DeepBlue a c
  | Something b
  deriving (Show, Eq)

instance Functor (Company a b) where
  fmap f (DeepBlue x y) = DeepBlue x (f y)
  fmap _ (Something z) = Something z

-- 3.
data More a b =
    L b a b
  | R a b a
  deriving (Show, Eq)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

-- Write Functor instances
-- 1.
data Quant a b = Finance
               | Desk a
               | Bloor b
               deriving (Show, Eq)

instance Functor (Quant a) where
  fmap _ Finance   = Finance
  fmap _ (Desk x)  = Desk x
  fmap f (Bloor y) = Bloor (f y)

-- 2.
data K a b = K a deriving (Show, Eq)

instance Functor (K a) where
  fmap _ (K x) = K x

-- 3.
newtype Flip f a b =
  Flip (f b a)
  deriving (Show, Eq)

newtype K' a b = K' a deriving (Show, Eq)

instance Functor (Flip K' a) where
  fmap f (Flip (K' x)) = Flip $ K' (f x)

-- 4.
data EvilGoateeConst a b =
  GoatyConst b
  deriving (Show, Eq)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst x) = GoatyConst (f x)

-- 5.
data LiftItOut f a =
  LiftItOut (f a)
  deriving (Show, Eq)

instance (Functor f) => Functor (LiftItOut f) where
  fmap f' (LiftItOut n) = LiftItOut (fmap f' n)

-- 6.
data Parappa f g a =
  DaWrappa (f a) (g a)
  deriving (Show, Eq)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f' (DaWrappa fa ga) = DaWrappa (fmap f' fa) (fmap f' ga)

-- 7.
data IgnoreOne f g a b =
  IgnoringSomething (f a) (g b)
  deriving (Show, Eq)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap f' (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f' gb)

-- 8.
data Notorius g o a t =
  Notorius (g o) (g a) (g t)
  deriving (Show, Eq)

instance (Functor g) => Functor (Notorius g o a) where
  fmap f (Notorius go ga gt) = Notorius go ga (fmap f gt)

-- 9.
data List a =
    Nil
  | Cons a (List a)
  deriving (Show, Eq)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x rem') = Cons (f x) (fmap f rem')

-- 10.
data GoatLord a =
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a)
              (GoatLord a)
              (GoatLord a)
  deriving (Show, Eq)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat x) = OneGoat (f x)
  fmap f (MoreGoats x y z) = MoreGoats (fmap f x)
                                       (fmap f y)
                                       (fmap f z)

-- 11.
data TalkToMe a =
    Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print str x) = Print str (f x)
  fmap f (Read sa) = Read (f . sa)

instance (Show a) => Show (TalkToMe a) where
  show Halt = "Halt"
  show (Print str x) = "Print " ++ str ++ " " ++ (show x)
  -- show (Read (f)) = "Read " ++ (show (f))

instance (Eq a) => Eq (TalkToMe a) where
  (==) Halt Halt = True
  (==) (Print str x) (Print str' x') = str == str' && x == x'
  (==) (Read f) (Read f') = f "3" == f' "3"

spec :: Spec
spec = do
  describe "Chapter Exercises" $ do
    it "2. works for BoolAndSomethingElse" $ do
      let f = (False' 12 :: BoolAndSomethingElse Int)
      fmap (+3) f `shouldBe` False' 15
      let t = True' "Hello"
      fmap (++"!") t `shouldBe` True' "Hello!"
    it "3. works for BoolAndMaybeSomethingElse" $ do
      fmap (+2) (Falsish :: BoolAndMaybeSomethingElse Int) `shouldBe` Falsish
      fmap (+2) (Truish 3 :: BoolAndMaybeSomethingElse Int) `shouldBe` Truish 5
    it "1. works with Sun a b" $ do
      fmap (+2) (First "Hello" :: Sum String Int) `shouldBe` First "Hello"
      fmap (+2) (Second 3 :: Sum String Int) `shouldBe` Second 5
    it "2. works with Company a b c" $ do
      fmap (+3) (DeepBlue "Hey" 2 :: Company String [Int] Int) `shouldBe` DeepBlue "Hey" 5
      fmap (+3) (Something [1] :: Company String [Int] Int) `shouldBe` Something [1]
    it "3. works with More a b" $ do
      fmap (+2) (L 3 "ho" 2 :: More String Int) `shouldBe` L 5 "ho" 4
      fmap (+2) (R "hey" 3 "ho" :: More String Int) `shouldBe` R "hey" 5 "ho"
    it "1. works with Quant a b" $ do
      fmap (+3) (Finance :: Quant String Int) `shouldBe` Finance
      fmap (+3) (Desk "yo" :: Quant String Int) `shouldBe` Desk "yo"
      fmap (+3) (Bloor 2 :: Quant String Int) `shouldBe` Bloor 5
    it "2. is meaningless with K type" $
      fmap (+2) (K "OK" :: K String Int) `shouldBe` K "OK"
    it "3. works with K' Flipped" $ do
      fmap (*3) (Flip (K' 2) :: Flip K' String Int) `shouldBe` Flip (K' 6)
    it "4. works with EvilGoateeConst a b" $
      fmap (*4) (GoatyConst 2 :: EvilGoateeConst String Int) `shouldBe` GoatyConst 8
    it "5. works with LiftItOut f a" $ do
      fmap (+3) (LiftItOut [1,2,3] :: LiftItOut [] Integer)
        `shouldBe` LiftItOut [4,5,6]
      fmap (+3) (LiftItOut (Just 3) :: LiftItOut Maybe Int)
        `shouldBe` LiftItOut (Just 6)
    it "6. works with Parappa f g a" $ do
      fmap (+3) (DaWrappa (Just 2) [1,2,3] :: Parappa Maybe [] Int)
        `shouldBe` DaWrappa (Just 5) [4,5,6]
    it "7. works with IgnoreOne" $ do
      fmap (+3) (IgnoringSomething (Just "Hey") [1,2,3] :: IgnoreOne Maybe [] String Int)
        `shouldBe` IgnoringSomething (Just "Hey") [4,5,6]
    it "8. works with Notorius" $ do
      fmap (+4) (Notorius (Just "hey") (Just "ho") (Just 3) :: Notorius Maybe String String Int)
        `shouldBe` Notorius (Just "hey") (Just "ho") (Just 7)
    it "9. works with lists" $ do
      let list = (Cons 3 (Cons 2 (Cons 1(Nil)))) :: List Int
      fmap (+2) list `shouldBe` (Cons 5(Cons 4(Cons 3(Nil))))
    it "10. works with recursive types" $ do
      let mg = MoreGoats NoGoat (OneGoat 12) (OneGoat 24) :: GoatLord Int
      fmap (+3) mg `shouldBe` MoreGoats NoGoat (OneGoat 15) (OneGoat 27)
    it "11. works with TalkToMe" $ do
      fmap (+3) (Halt :: TalkToMe Int) `shouldBe` Halt
      fmap (+3) (Print "Hello" 3 :: TalkToMe Int) `shouldBe` Print "Hello" 6
      let (Read f) = fmap (+3) (Read (\x -> read x :: Int))
      f "2" `shouldBe` 5
