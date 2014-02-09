module Data.CPE (CPERecord) where

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
                 (part cpe) (vendor cpe) (Data.CPE.product cpe) (version cpe)
                 (update cpe) (edition cpe) (language cpe)

instance Read CPERecord where
    readsPrec _ cpeString =
        case split ':' cpeString of
          [_, part, vendor, product, version, update, edition, language]
              -> [(CPERecord (tail part) vendor product 
                             version (read update) edition language,[])]
          x -> error $ "Wrong format in CPE String: " ++ show x
        where
          split :: Eq a => a -> [a] -> [[a]]
          split _ [] = []
          split delim l = let (init,remainder) = span (/= delim) l
                          in init : case remainder of
                                      [] -> []
                                      x -> split delim (tail remainder)
