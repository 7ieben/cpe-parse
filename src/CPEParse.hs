module CPEParse where

-- cpe:/{part}:{vendor}:{product}:{version}:{update}:{edition}:{language}

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

-- Just a simple State Monad
newtype ParseState s a = ParseState { runState :: s -> (a,s) }

instance Monad (ParseState s) where
    return a = ParseState $ \s -> (a, s)
    m >>= k = ParseState $ \s -> let (a, s') = runState m s
                                 in runState (k a) s'

-- Takes a String (representing the still to pasre CPE String)
-- returns a pair of 1: the Value of the next Section as a String
--                   2: the rest of the CPE String as the new State
parseSection :: ParseState String String
parseSection = ParseState (\s -> (takeWhile (/= ':') s, 
                                  drop 1 $ dropWhile (/= ':') s)) 

-- cpe = "a:oracle:db:12c:1:enterprice:de"
stringToCPEPart :: String -> CPEPart
stringToCPEPart s = case s of
                      "h" -> H
                      "a" -> A
                      "o" -> O
