module Hpffp.TraversableInstancesSpec where

import Test.Hspec
import Data.Foldable
import Data.Monoid

main :: IO ()
main = hspec spec

data TEither a b =
      TLeft a
    | TRight b
    deriving (Eq, Ord, Show)

instance Functor (TEither a) where
    fmap _ (TLeft x) = TLeft x
    fmap f (TRight y) = TRight (f y)

instance Applicative (TEither e) where
    pure           = TRight
    TLeft e <*> _  = TLeft e
    TRight f <*> r = fmap f r

instance Foldable (TEither a) where
    foldMap _ (TLeft _) = mempty
    foldMap f (TRight y)  = f y

    foldr _ z (TLeft _) = z
    foldr f z (TRight y) = f y z

instance Traversable (TEither a) where
    traverse _ (TLeft x) = pure (TLeft x)
    traverse f (TRight y) = TRight <$> f y

spec :: Spec
spec = do
    describe "Traversable Instances - Either" $ do
        it "works for Functor" $ do
            fmap (++"!") (TLeft "Hey") `shouldBe` TLeft "Hey"
            fmap (+3) (TRight 2 :: TEither String Int)
                `shouldBe` TRight 5
        it "works for Applicative" $ do
            TLeft "Hey" <*> TLeft "!" `shouldBe`
                (TLeft "Hey" :: TEither String Int)
            TRight (++"!") <*> TRight "Yo"
                `shouldBe` (TRight "Yo!" :: TEither Int String)
        it "works as Foldable" $ do
            foldMap (++"!") (TLeft "Yo") `shouldBe` ""
            foldMap (++[0]) (TLeft [2,3]) `shouldBe` []
            foldMap (++[100]) (TRight [3,4]) `shouldBe` [3,4,100]
            foldr (\x acc -> x ++ acc) "?" (TRight "Hey") `shouldBe` "Hey?"
            foldr (\x acc -> x + acc) 0 (TRight 4) `shouldBe` 4
        it "works as Traversable" $ do
            traverse (++"!") (TLeft 3 :: TEither Int String)
                `shouldBe` [TLeft 3]
            traverse (++"!") (TRight "Yo" :: TEither Int String)
                `shouldBe` [TRight 'Y', TRight 'o', TRight '!']
