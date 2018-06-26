module Hpffp.SignalingAdversity where

import Control.Applicative (liftA2)

type Name = String

type Age = Integer

data PersonInvalid
    = NameEmpty
    | AgeTooLow

type ValidatePerson a = Either [PersonInvalid] a

-- Compiles without Eq
toString :: PersonInvalid -> String
toString NameEmpty = "NameEmpty"
toString AgeTooLow = "AgeTooLow"

instance Show PersonInvalid where
    show = toString

data Person =
    Person Name
           Age
    deriving (Show)

mkPerson :: Name -> Age -> Maybe Person
mkPerson name age
    | name /= "" && age >= 0 = Just $ Person name age
    | otherwise = Nothing

ageOkay :: Age -> Either [PersonInvalid] Age
ageOkay age =
    case age >= 0 of
        True -> Right age
        False -> Left [AgeTooLow]

nameOkay :: Name -> Either [PersonInvalid] Name
nameOkay name =
    case name /= "" of
        True -> Right name
        False -> Left [NameEmpty]

mkPerson' :: Name -> Age -> ValidatePerson Person
mkPerson' name age =
    mkPerson'' (nameOkay name) (ageOkay age)
    -- liftA2 Person (nameOkay name) (ageOkay age)

mkPerson'' :: ValidatePerson Name -> ValidatePerson Age -> ValidatePerson Person
mkPerson'' (Right nameOk) (Right ageOk) = Right (Person nameOk ageOk)
mkPerson'' (Left badName) (Left badAge) = Left (badName ++ badAge)
mkPerson'' (Left badName) _ = Left badName
mkPerson'' _ (Left badAge) = Left badAge
