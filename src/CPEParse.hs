module CPEParse where

-- cpe:/{part}:{vendor}:{product}:{version}:{update}:{edition}:{language}

data CPEPart = H  --Hardware
             | A  --Application
             | O  --Operatingsystem

data CPERecord = CPERecord
    { part :: CPEPart
    , vendor :: String
    , product :: String
    , version :: String
    , update :: Int
    , edition :: String
    , language :: String
    }

data CPEParseState = CPEParseState {
      restString :: String
    } deriving (Show)

initState :: String -> CPEParseState
initState s = CPEParseState s

parsePart :: CPEParseState -> (String, CPEParseState)
parsePart ps = (val, newState)
    where
      (val, rest) = span (\x -> x /= ':') $ restString ps
      newState = CPEParseState {restString = drop 1 rest}

isEOS :: String -> Bool
isEOS = (==) ""            
