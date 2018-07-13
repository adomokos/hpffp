{-# LANGUAGE FlexibleInstances #-}
module Hpffp.FunctorExercisesSpec where

import Test.Hspec

main :: IO ()
main = hspec spec

-- 1.

data Sum a b =
    First a
  | Second b
  deriving (Show, Eq)

instance Functor (Sum e) where
    fmap _ (First a) = First a
    fmap f (Second b) = Second (f b)

-- 2.
data Company a b c =
    DeepBlue a c
  | Something b
  deriving (Show, Eq)

instance Functor (Company a b) where
    fmap f (DeepBlue x y) = DeepBlue x (f y)
    fmap _ (Something b) = Something b

-- 3.
data More a b =
    L a b a
  | R b a b
  deriving (Show, Eq)

instance Functor (More x) where
    fmap f (L x y z) = L x (f y) z
    fmap f (R x y z) = R (f x) y (f z)

-- 4.
data Quant a b =
    Finance
  | Desk a
  | Bloor b
  deriving (Show, Eq)

instance Functor (Quant a) where
    fmap _ Finance = Finance
    fmap _ (Desk x) = Desk x
    fmap f (Bloor y) = Bloor (f y)

-- 5.
data K a b = K a deriving (Show, Eq)

instance Functor (K a) where
    fmap _ (K x) = K x

-- 6.

newtype Flip f a b =
    Flip (f b a)
    deriving (Show, Eq)

{- newtype K a b = K a -}

instance Functor (Flip K a) where
    fmap f (Flip (K b)) = Flip (K (f b))

-- 7.
data EvilGoateeConst a b =
    GoatyConst b

instance Functor (EvilGoateeConst a) where
    fmap f (GoatyConst b) = GoatyConst (f b)

-- 8.
data LiftItOut f a =
    LiftItOut (f a) deriving (Show, Eq)

instance Functor f => Functor (LiftItOut f) where
    fmap f (LiftItOut fa) = LiftItOut (fmap f fa)

-- 9.

data Parappa f g a =
    DaWrappa (f a) (g a)
    deriving (Show, Eq)

instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

-- 10.

data IgnoreOne f g a b =
    IgnoringSomething (f a) (g b)
    deriving (Show, Eq)

instance (Functor f, Functor g) => Functor (IgnoreOne f g a) where
    fmap f (IgnoringSomething fa gb) =
        IgnoringSomething fa (fmap f gb)

-- 11.
data Notorious g o a t =
    Notorious (g o) (g a) (g t)
    deriving (Show, Eq)

instance Functor g => Functor (Notorious g o a) where
    fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

-- 12.
data List a =
    Nil
  | Cons a (List a)
  deriving (Show,Eq)

instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons a list) = Cons (f a) (fmap f list)

-- 13.
data GoatLord a = NoGoat
                 | OneGoat a
                 | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
                 deriving (Show, Eq)

instance Functor GoatLord where
    fmap _ NoGoat = NoGoat
    fmap f (OneGoat x) = OneGoat (f x)
    fmap f (MoreGoats gl gl' gl'')
      = MoreGoats (fmap f gl) (fmap f gl') (fmap f gl'')

-- 14.
data TalkToMe a = Halt
                | Print String a
                | Read (String -> a)

instance Functor TalkToMe where
    fmap _ Halt = Halt
    fmap f (Print s a) = Print s (f a)
    fmap f (Read sa) = Read (f . sa)


spec :: Spec
spec = do
    describe "Functor Exercises" $ do
        it "fixes types" $ do
            fmap (+1) ((Second 2) :: Sum String Int)
                `shouldBe` Second 3
            fmap (+1) ((DeepBlue "Hello" 2) :: Company String Bool Int)
                `shouldBe` (DeepBlue "Hello" 3)
            fmap (+2) ((L 'h' 3 'z') :: More Char Int)
                `shouldBe` (L 'h' 5 'z')
        it "can have functor instances for complex types" $ do
            fmap (+2) (Finance :: Quant String Int) `shouldBe` Finance
            fmap (+2) (Desk "yay" :: Quant String Int) `shouldBe` Desk "yay"
            fmap (+2) (Bloor 3 :: Quant String Int) `shouldBe` Bloor 5
            fmap (+2) (K 3 :: K Int Int) `shouldBe` K 3
            fmap (+2) (LiftItOut [2,3,4] :: LiftItOut [] Integer)
                `shouldBe` LiftItOut [4,5,6]
            fmap (+3) (DaWrappa [2,3,4] [5,6,7])
                `shouldBe` DaWrappa [5,6,7] [8,9,10]
            fmap (+2) (IgnoringSomething [2,3] [4,5])
                `shouldBe` IgnoringSomething [2,3] [6,7]
            fmap (+2) (Notorious "he" [1,2] [3,4])
                `shouldBe` Notorious "he" [1,2] [5,6]
