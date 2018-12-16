module CH16.ReplaceExperimentSpec where

import Test.Hspec

main :: IO ()
main = hspec spec

replaceWithP :: b -> Char
replaceWithP = const 'p'

lms :: [Maybe [Char]]
lms = [Just "Ave", Nothing, Just "woohoo"]

replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP' = replaceWithP

liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP

liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace

twiceLifted :: (Functor f1, Functor f) =>
                f (f1 a) -> f (f1 Char)
twiceLifted = (fmap . fmap) replaceWithP

-- Making it more specific
twiceLifted' :: [Maybe [Char]] -> [Maybe Char]
twiceLifted' = twiceLifted

thriceLifted :: (Functor f2, Functor f1, Functor f)
              => f (f1(f2 a)) -> f (f1 (f2 Char))
thriceLifted = (fmap . fmap . fmap) replaceWithP

-- More specific or "concrete"
thriceLifted' :: [Maybe [Char]] -> [Maybe [Char]]
thriceLifted' = thriceLifted

spec :: Spec
spec = do
  describe "Tri-lifting" $ do
    it "works with custom type" $ do
      replaceWithP' lms `shouldBe` 'p'
      liftedReplace lms `shouldBe` "ppp"
      liftedReplace lms `shouldBe` "ppp"
      twiceLifted lms `shouldBe` [Just 'p', Nothing, Just 'p']
      twiceLifted' lms `shouldBe` [Just 'p', Nothing, Just 'p']
      thriceLifted lms `shouldBe` [Just "ppp", Nothing, Just "pppppp"]
      thriceLifted' lms `shouldBe` [Just "ppp", Nothing, Just "pppppp"]
