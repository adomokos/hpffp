module ADT.VehiclesSpec where

import Test.Hspec
import Control.Exception (evaluate)

data Price = Price Integer deriving (Eq, Show)
data Manufacturer = Mini
                  | Mazda
                  | Tata
                    deriving (Eq, Show)

data Airline = PapuAir
             | CatapultsR'Us
             | TakeYourChancesUnited
             deriving (Eq, Show)

type Size = Int

data Vehicle = Car Manufacturer Price
             | Plane Airline Size
             deriving (Eq, Show)

main :: IO ()
main = hspec spec

myCar :: Vehicle
myCar = Car Mini (Price 14000)

urCar :: Vehicle
urCar = Car Mazda (Price 20000)

clownCar :: Vehicle
clownCar = Car Tata (Price 7000)

doge :: Vehicle
doge = Plane PapuAir 45

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _         = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _         = False

areCars :: [Vehicle] -> [Bool]
areCars = map (isCar)

getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m
getManu _         = error "Not a car" -- Bad implementation

spec :: Spec
spec = do
    describe "ADT - Vehicles" $ do
        it "demonstrates them" $ do
          isCar myCar `shouldBe` True
          isCar urCar `shouldBe` True
          isCar doge `shouldBe` False
          isPlane doge `shouldBe` True
          isPlane myCar `shouldBe` False
          areCars [myCar, urCar, doge]
            `shouldBe` [True, True, False]
        it "can tell the manufacturer of a Car" $ do
          getManu myCar `shouldBe` Mini
          evaluate(getManu doge) `shouldThrow` anyErrorCall

