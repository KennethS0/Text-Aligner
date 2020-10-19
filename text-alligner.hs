-- Kenneth Sánchez Ocampo

-- Datatype definitions
-- Line: is a synonim to a list of tokens
type Line = [Token]

-- Token: Can take 3 forms Word ("abc"), "Blank" (" "), or "HypWord" ("abc-") 
data Token = Word String | Blank | HypWord String
             deriving (Eq, Show)

-- Show for each different token
showToken :: Token -> String
showToken (Word(a)) = a ++ " "
showToken (HypWord(a)) = a ++ "- " 
showToken Blank = " "


-- trim:
-- Gets rid of trailing spaces
trim :: String -> String
trim x | head x == ' ' = trim (tail x)
       | last x == ' ' = trim (init x)
       | otherwise = x


-- string2line: 
-- Takes a string as input and returns a Line
string2line :: String -> Line
string2line str = [Word x | x <- words str]


-- line2string
-- Takes a line as input and returns a string
line2string :: Line -> String
line2string [] = ""
line2string (x:xs) = showToken x ++ line2string xs