module CPEParse 
    ( CPEPart
    , CPERecord
    , parseCPE
    ) where

-- Datatypes for representing a CPE Record
data CPEPart = H  --Hardware
             | A  --Application
             | O  --Operatingsystem
               deriving (Show)

data CPERecord = CPERecord
    { part :: CPEPart
    , vendor :: String
    , product :: String
    , version :: String
    , update :: Int
    , edition :: String
    , language :: String
    } deriving (Show)

stringToCPEPart :: String -> CPEPart
stringToCPEPart s = case s of
                      "h" -> H
                      "a" -> A
                      "o" -> O

-- from Data.List.Split
splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ [] = []
splitOn f l@(x:xs)
  | f x = splitOn f xs
  | otherwise = let (h,t) = break f l in h:(splitOn f t)

parseCPE :: String -> CPERecord
parseCPE s = CPERecord part vendor product version update edition language
    where
      cpeList = splitOn (== ':') $ drop 5 s
      part = stringToCPEPart $ cpeList !! 0
      vendor = cpeList !! 1
      product = cpeList !! 2
      version = cpeList !! 3
      update = read $ cpeList !! 4
      edition = cpeList !! 5
      language = cpeList !! 6
