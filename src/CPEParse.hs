module CPEParse 
    ( CPERecord
    , parseCPE
    ) where

import Text.Printf (printf)

data CPERecord = CPERecord
    { part :: String
    , vendor :: String
    , product :: String
    , version :: String
    , update :: Int
    , edition :: String
    , language :: String
    } deriving (Eq, Ord)

instance Show CPERecord where
    show cpe = printf "cpe:/%s:%s:%s:%s:%d:%s:%s"
                 (part cpe) (vendor cpe) (CPEParse.product cpe) (version cpe)
                 (update cpe) (edition cpe) (language cpe)

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
      part = cpeList !! 0
      vendor = cpeList !! 1
      product = cpeList !! 2
      version = cpeList !! 3
      update = read $ cpeList !! 4
      edition = cpeList !! 5
      language = cpeList !! 6
