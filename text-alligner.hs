-- Kenneth Sánchez Ocampo

import Data.Map

-- Datatype definitions
-- Line: is a synonim to a list of tokens
type Line = [Token]


-- HypMap: Maps a word with its syllable separation  ()
type HypMap = Data.Map.Map String [String]


-- Token: Can take 3 forms Word ("abc"), "Blank" (" "), or "HypWord" ("abc-") 
data Token = Word String | Blank | HypWord String
             deriving (Eq, Show)


-- Show for each different token
show' :: Token -> String
show' (Word(a)) = a
show' (HypWord(a)) = a ++ "-" 
show' Blank = " "


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


-- line2string:
-- Takes a line as input and returns a string
line2string :: Line -> String
line2string [] = ""
line2string [x] | x == Blank = ""
                | otherwise = show' x
line2string (x:xs) | x == Blank = show' x ++ line2string xs
                   | otherwise = show' x ++ " " ++ line2string xs

-- tokenLenght:
-- Obtains the length of each type of token.
tokenLength :: Token -> Int
tokenLength (Word(a)) = length a
tokenLength (HypWord(a)) = 1 + length a
tokenLength Blank = 1


-- lineLength:
-- Obtains the length of a line
lineLength :: Line -> Int
lineLength [] = 0
lineLength [x] = tokenLength x
lineLength (x:xs) = tokenLength x + 1 + lineLength xs


-- TODO breakLine:
-- Breaks the line at a given index and returns a tuple
breakLine :: Int -> Line -> (Line, Line)
breakLine 0 line = ([], line)


-- TODO mergers:
-- Returns all possible ways of parting a string, keeping the same order
mergers :: [String] -> [(String, String)]
mergers [x] = []


-- TODO hyphenate:
-- Separates a Word into a HypWord and another Word
-- hyphenate :: HypMap -> Token -> [(Toke, Token)]


-- TODO lineBreaks:
-- Finds the different ways in which a line can be separated, given
-- a specific number of characters.
-- lineBreaks :: HypMap -> Int -> Line -> [(Line, Line)]


-- TODO insertBlanks:
-- Distribuites a set amount of blanks between the words of a line.
-- insertBlanks :: Line -> Line


-- TODO separarYalinear:
-- Separates and alligns the text