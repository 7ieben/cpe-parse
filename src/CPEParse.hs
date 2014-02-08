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

split :: Eq a => a -> [a] -> [[a]]
split _ [] = []
split delim l = let (init,remainder) = span (/= delim) l
                in init : case remainder of
                            [] -> []
                            x -> split delim (tail remainder)

parseCPE :: String -> CPERecord
parseCPE s = CPERecord part vendor product version update edition language
    where
      cpeList = split ':' $ drop 5 s
      part = stringToCPEPart $ cpeList !! 0
      vendor = cpeList !! 1
      product = cpeList !! 2
      version = cpeList !! 3
      update = read $ cpeList !! 4
      edition = cpeList !! 5
      language = cpeList !! 6
