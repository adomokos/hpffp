module Hpffp.ExamplesOfMonadsSpec where

import Test.Hspec

main :: IO ()
main = hspec spec

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
    x <- xs
    if even x
       then [x*x, x*x]
       else [x*x]

twiceWhenEven' :: [Integer] -> [Integer]
twiceWhenEven' xs = do
    x <- xs
    if even x
       then [x*x, x*x]
       else []

data Cow = Cow {
      name :: String
    , age :: Int
    , weight :: Int
               } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n
    | n >= 0 = Just n
    | otherwise = Nothing

-- if Cow's name is Bess, must be under 500
weightCheck :: Cow -> Maybe Cow
weightCheck c =
    let w = weight c
        n = name c
    in if n == "Bess" && w > 499
          then Nothing
          else Just c

mkSphericalCow :: String
               -> Int
               -> Int
               -> Maybe Cow
mkSphericalCow name' age' weight' =
    case noEmpty name' of
      Nothing -> Nothing
      Just nammy ->
          case noNegative age' of
            Nothing -> Nothing
            Just agey ->
                case noNegative weight' of
                  Nothing -> Nothing
                  Just weighty -> weightCheck (Cow nammy agey weighty)

mkSphericalCow' :: String
                -> Int
                -> Int
                -> Maybe Cow
mkSphericalCow' name' age' weight' = do
    nammy <- noEmpty name'
    agey <- noNegative age'
    weighty <- noNegative weight'
    weightCheck (Cow nammy agey weighty)

mkSphericalCow'' :: String
                 -> Int
                 -> Int
                 -> Maybe Cow
mkSphericalCow'' name' age' weight' = do
    noEmpty name' >>=
        \nammy ->
        noNegative age' >>=
        \agey ->
        noNegative weight' >>=
        \weighty ->
        weightCheck (Cow nammy agey weighty)


-- ******************
-- For Either e monad

-- years ago
type Founded = Int
-- number of programs
type Coders = Int

data SoftwareShop =
    Shop {
        founded :: Founded
      , programmers :: Coders
    } deriving (Show, Eq)

data FoundedError =
      NegativeYears Founded
    | TooManyYears Founded
    | NegativeCoders Coders
    | TooManyCoders Coders
    | TooManyCodersForYears Founded Coders
    deriving (Eq, Show)

validateFounded :: Int
    -> Either FoundedError Founded
validateFounded n
    | n < 0 = Left $ NegativeYears n
    | n > 500 = Left $ TooManyYears n
    | otherwise = Right n

-- Tho, many programmers *are* negative
validateCoders :: Int -> Either FoundedError Coders
validateCoders n
    | n < 0 = Left $ NegativeCoders n
    | n > 5000 = Left $ TooManyCoders n
    | otherwise = Right n

mkSoftware :: Int -> Int -> Either FoundedError SoftwareShop
mkSoftware years coders = do
    founded <- validateFounded years
    programmers <- validateCoders coders
    if programmers > div founded 10
       then Left $ TooManyCodersForYears founded programmers
       else Right $ Shop founded programmers

-- Exercise: implement the Either monad
data Sum a b =
    First a | Second b deriving (Show, Eq)

instance Functor (Sum a) where
    fmap _ (First x) = First x
    fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
    pure = Second
    (<*>) _ (First x) = First x
    (<*>) (First x) _ = First x
    (<*>) (Second f) (Second y) = Second (f y)

instance Monad (Sum a) where
    return = pure
    First x >>= _ = First x
    Second x >>= f = f x

spec :: Spec
spec = do
    describe "Monad Examples" $ do
        it "works with Lists" $ do
            {-
                (>>=) :: [a] -> (a -> [b]) -> [b]
                -- return is the same as pure
                return :: a -> [a]
            -}
            let result = concat ([if (even x) then [x*x, x*x] else [x*x] | x <- [1..3]])
            twiceWhenEven [1..3] `shouldBe` [1,4,4,9]
            result `shouldBe` [1,4,4,9]
            twiceWhenEven' [1..3] `shouldBe` [4,4]
        it "works with Maybe - the verbose example" $ do
            let cow = mkSphericalCow "Bess" 5 499
            cow `shouldBe` Just (Cow "Bess" 5 499)
            let cow' = mkSphericalCow "Bess" 5 500
            cow' `shouldBe` Nothing
        it "works with Maybe - the shorter examples with dos" $ do
            let cow = mkSphericalCow' "Bess" 5 499
            cow `shouldBe` Just (Cow "Bess" 5 499)
            let cow' = mkSphericalCow' "Bess" 5 500
            cow' `shouldBe` Nothing
        it "works with Either" $ do
            mkSoftware 0 0 `shouldBe` Right (Shop 0 0)
            mkSoftware (-1) 0 `shouldBe` Left (NegativeYears (-1))
            mkSoftware (-1) (-1) `shouldBe` Left (NegativeYears (-1))
            mkSoftware (0) (-1) `shouldBe` Left (NegativeCoders (-1))
            mkSoftware 500 0 `shouldBe` Right (Shop 500 0)
            mkSoftware 501 0 `shouldBe` Left (TooManyYears 501)
            mkSoftware 501 501 `shouldBe` Left (TooManyYears 501)
            mkSoftware 100 5001 `shouldBe` Left (TooManyCoders 5001)
            mkSoftware 0 500 `shouldBe` Left (TooManyCodersForYears 0 500)
        it "works with the custom Either type" $ do
            fmap (*2) (First "hello" :: Sum String Int) `shouldBe` First "hello"
            fmap (*2) (Second 4 :: Sum String Int) `shouldBe` Second 8
            (*) <$> (Second 2 :: Sum String Int) <*> Second 4
                `shouldBe` Second 8
            (*) <$> (First "no" :: Sum String Int) <*> Second 4
                `shouldBe` First "no"
            ((Second 2 :: Sum String Int) >>= \x -> Second (x*2))
                `shouldBe` Second 4
            ((First "nope" :: Sum String Int) >>= \x -> Second (x*2))
                `shouldBe` First "nope"
