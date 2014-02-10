module Text.CPE where

import Text.ParserCombinators.Parsec

cpeFile = endBy cpe eol
cpe = sepBy field (char ':')
field = many (noneOf ":\n")
eol = char '\n'

parseCPE :: String -> Either ParseError [[String]]
parseCPE input = parse cpeFile "" input
