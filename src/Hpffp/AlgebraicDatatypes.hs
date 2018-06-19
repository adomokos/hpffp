{-# LANGUAGE FlexibleInstances #-}
module Hpffp.AlgebraicDatatypes where

data Price =
    Price Integer
    deriving (Eq, Show)

data Size = Size Integer deriving (Eq, Show)

data Manufacturer
    = Mini
    | Mazda
    | Tata
    deriving (Eq, Show)

data Airline
    = PapuAir
    | CatapultsR'Us
    | TakeYourChancesUnited
    deriving (Eq, Show)

data Vehicle
    = Car Manufacturer
          Price
    | Plane Airline Size
    deriving (Show, Eq)

myCar :: Vehicle
myCar = Car Mini (Price 14000)

urCar :: Vehicle
urCar = Car Mazda (Price 20000)

clownCar :: Vehicle
clownCar = Car Tata (Price 7000)

doge :: Vehicle
doge = Plane PapuAir (Size 300)

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map (isCar)

getManu :: Vehicle -> Manufacturer
getManu (Car manu _) = manu

class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 42

newtype Goats = Goats Int deriving Show

instance TooMany Goats where
    tooMany (Goats n) = n > 43

class TooMany' a where
    tooMany' :: a -> Bool

instance TooMany' Int where
    tooMany' n = n > 42

newtype Goats' = Goats' Int deriving (Eq, Show)

instance TooMany' (Int, String) where
    tooMany' (x, _) = x > 42

{-
-- This is not needed, just use
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
newtype Goats' = Goats' Int deriving (Eq, Show, TooMany')

instance TooMany' Goats' where
    tooMany' (Goats' n) = tooMany' n
-}
