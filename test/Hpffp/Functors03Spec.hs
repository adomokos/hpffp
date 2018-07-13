module Hpffp.Functors03Spec where

import Test.Hspec

main :: IO ()
main = hspec spec

incIfJust :: Num a => Maybe a -> Maybe a
incIfJust (Just n) = Just $ n + 1
incIfJust Nothing = Nothing

showIfJust :: Show a => Maybe a -> Maybe String
showIfJust (Just s) = Just $ show s
showIfJust Nothing = Nothing

incMaybe :: Num a => Maybe a -> Maybe a
incMaybe m = fmap (+1) m

showMaybe :: Show a => Maybe a -> Maybe String
showMaybe s = fmap show s

-- It can be made more generic
incMaybe'' :: Num a => Maybe a -> Maybe a
incMaybe'' = fmap (+1)

showMaybe'' :: Show a => Maybe a -> Maybe String
showMaybe'' = fmap show

-- Lifted version
liftedInc :: (Functor f, Num b) => f b -> f b
liftedInc = fmap (+1)

liftedShow :: (Functor f, Show a) => f a -> f String
liftedShow = fmap show

data Possibly a =
      LolNope
    | Yeppers a
    deriving (Show, Eq)

instance Functor Possibly where
    fmap _ LolNope = LolNope
    fmap f (Yeppers x) = Yeppers (f x)

spec :: Spec
spec = do
    describe "More Functors" $ do
        it "works with Maybe" $ do
            incMaybe (Just 1) `shouldBe` Just 2
            incMaybe Nothing `shouldBe` Nothing
            showMaybe (Just 9001) `shouldBe` Just "9001"
            showMaybe (Nothing :: Maybe String) `shouldBe` Nothing
        it "with eta-recuded functions" $ do
            incMaybe'' (Just 1) `shouldBe` Just 2
            incMaybe'' Nothing `shouldBe` Nothing
            showMaybe'' (Just 9001) `shouldBe` Just "9001"
        it "uses the lifted type declaration, more reusable" $ do
            liftedInc (Just 1) `shouldBe` Just 2
            liftedInc [1..5] `shouldBe` [2..6]
            liftedShow (Nothing :: Maybe Int) `shouldBe` Nothing
            liftedShow [1..3] `shouldBe` ["1","2","3"]
        it "can use Possibly in a Functor context" $ do
            fmap (+2) LolNope `shouldBe` LolNope
            fmap (+3) (Yeppers 4) `shouldBe` Yeppers 7
