-- Kenneth Sánchez Ocampo

-- Datatype definitions
-- Line: is a synonim to a list of tokens
type Line = [Token]

-- Token: Can take 3 forms Word ("abc"), "Blank" (" "), or "HypWord" ("abc-") 
data Token = Word String | Blank | HypWord String
             deriving (Eq,Show)

-- string2line: 
-- Takes a string as input and returns a Line
string2line :: String -> Line
string2line "" = []
string2line (char:chars) | char == ' ' = [Blank] ++ string2line chars
                         | otherwise = [Word [char]] ++ string2line chars