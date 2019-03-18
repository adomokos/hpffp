module CH22.FunctionApplicativeSpec where

import Test.Hspec
import Control.Applicative (liftA2)

main :: IO ()
main = hspec spec

newtype HumanName =
  HumanName String
  deriving (Eq, Show)

newtype DogName =
  DogName String
  deriving (Eq, Show)

newtype Address =
  Address String
  deriving (Eq, Show)

data Person =
  Person {
    humanName :: HumanName
  , dogName :: DogName
  , address :: Address
  } deriving (Show, Eq)

data Dog =
  Dog {
    dogsName :: DogName
  , dogsAddress :: Address
  } deriving (Show, Eq)

pers :: Person
pers =
  Person (HumanName "Big Bird")
         (DogName "Barkley")
         (Address "Sesame Street")

john :: Person
john =
  Person (HumanName "John Smith")
         (DogName "Papu")
         (Address "Austin")

-- without Reader
getDog :: Person -> Dog
getDog p =
  Dog (dogName p) (address p)

-- with Reader
getDogR :: Person -> Dog
getDogR =
  Dog <$> dogName <*> address

{-
  Can't see the Reader? What if we concrete the types a bit?


-}

(<$->>) :: (a -> b)
        -> ((->) r) a
        -> ((->) r) b
(<$->>) = (<$>)


(<*->>) :: ((->) r) (a -> b)
        -> ((->) r) a
        -> ((->) r) b
(<*->>) = (<*>)

-- with Reader
getDogR' :: Person -> Dog
getDogR' =
  Dog <$->> dogName <*->> address

getDogRA2 :: Person -> Dog
getDogRA2 = liftA2 Dog dogName address

spec :: Spec
spec = do
  describe "Functions are Applicative" $ do
    it "works like one" $ do
      getDog pers
        `shouldBe` Dog (DogName "Barkley") (Address "Sesame Street")
      getDogR john
        `shouldBe` Dog (DogName "Papu") (Address "Austin")
      getDogR' john
        `shouldBe` Dog (DogName "Papu") (Address "Austin")

