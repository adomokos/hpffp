module CH16.ExercisesSpec where

import Test.Hspec

main :: IO ()
main = hspec spec

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = read <$> (fmap ("123"++) (fmap show ioi))
    in (*3) <$> changed

data Sum a b = First a
             | Second b
             deriving (Show, Eq)

instance Functor (Sum a) where
  fmap _ (First x) = First x
  fmap f (Second y) = Second (f y)

spec :: Spec
spec = do
  describe "Exercises" $ do
    it "1." $ do
      let a = fmap (+1) (read "[1]" :: [Int])
      a `shouldBe` [2]
    it "2." $ do
      let b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])
      b `shouldBe` Just ["Hi,lol","Hellolol"]
    it "3." $ do
      let c = (*2) <$> (\x -> x - 2)
      c 1 `shouldBe` -2
    it "4." $ do
      let d = fmap ((return '1' ++) . show)
              (\x -> [x, 1..3])
      d 0 `shouldBe` "1[0,1,2,3]"
    it "5." $ do
      x <- e
      x `shouldBe` 3693
    it "works with custom Either type" $ do
      let left = First "hey"
      fmap (*2) left `shouldBe` left
      let right = Second 3
      fmap (*2) right `shouldBe` (Second 6 :: Sum String Int)
