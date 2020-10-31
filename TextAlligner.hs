-- Kenneth Sánchez Ocampo
module TextAlligner where

import Data.Map
import Data.List
import Data.Char

-- Datatype definitions
-- Line: is a synonim to a list of tokens
type Line = [Token]

-- HypMap: Maps a word with its syllable separation  ()
type HypMap = Data.Map.Map String [String]

-- Token: Can take 3 forms Word ("abc"), "Blank" (" "), or "HypWord" ("abc-") 
data Token = Word String | Blank | HypWord String
             deriving (Eq, Show)

-- Flag: Determines if a line will be separated or adjusted.
data Flag = SEPARAR | NOSEPARAR | AJUSTAR | NOAJUSTAR


-- show':
-- Turns each token into a string
show' :: Token -> String
show' (Word a) = a
show' (HypWord a) = a ++ "-" 
show' Blank = " "


-- trim:
-- Gets rid of trailing spaces
trim :: String -> String
trim [] = []
trim x | head x == ' ' = trim (tail x)
       | last x == ' ' = trim (init x)
       | otherwise = x


-- string2line: 
-- Takes a string as input and returns a Line
string2line :: String -> Line
string2line str = [Word x | x <- words str]


-- combine:
-- Turns all the Tokens in a Line and turns them into a string
combine :: Line -> String
combine [] = ""
combine [x] | x == Blank = ""
            | otherwise = show' x
combine (x:xs) | x == Blank = show' x ++ combine xs
               | otherwise = show' x ++ " " ++ combine xs


-- line2string:
-- Turns a Line into a string
line2string :: Line -> String
line2string xs = trim(combine xs)


-- tokenLength:
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


-- getSepIndex:
-- Obtains the index of a separation
getSepIndex :: Int -> Int -> Line -> Int
getSepIndex ind _ [] = ind
getSepIndex ind len (x:xs) | len < tokenLength x = ind
                           | otherwise = getSepIndex (ind + 1) (len - (tokenLength x) - 1) xs 


-- breakLine:
-- Breaks the line at a given index and returns a tuple
breakLine :: Int -> Line -> (Line, Line)
breakLine _ [] = ([], [])
breakLine a xs = Data.List.splitAt (getSepIndex 0 a xs) xs


-- mergers:
-- Returns all possible ways of combining a list of strings, keeping the same order
mergers :: [String] -> [(String, String)]
mergers xs = [(concat (fst x), concat (snd x)) | x <- zip (inits xs) (tails xs), fst x /= [], snd x /= []]


-- removeMaybe:
-- Removes the Maybe from a list of strings
removeMaybe :: Maybe [String] -> [String]
removeMaybe (Just x) = x 
removeMaybe Nothing = []


-- removePunctuation:
-- Removes any type of punctuation in a string
removePunctuation :: String -> String
removePunctuation xs | isAlpha (last xs) = xs
                     | otherwise = removePunctuation (init xs)


-- getPunctuation:
-- Obtains the punctuation of a word
getPunctuation :: String -> String
getPunctuation xs | isAlpha (last xs) = ""
                  | otherwise = getPunctuation (init xs) ++ [last xs]


-- hyphenate:
-- Separates a Word into a HypWord and another Word
hyphenate :: HypMap -> Token -> [(Token, Token)]
hyphenate map a = [(HypWord (fst x), Word (snd x ++ b)) 
                  | x <- mergers (removeMaybe (Data.Map.lookup (removePunctuation(show' a)) map))]
                  where
                         b = getPunctuation (show' a)


-- TODO lineBreaks:
-- Finds the different ways in which a line can be separated, given
-- a specific number of characters.
lineBreaks :: HypMap -> Int -> Line -> [(Line, Line)]
lineBreaks map a xs | a > lineLength xs = [(xs, [])]
                    | otherwise = [lines] ++ [(fst lines ++ [fst x], [snd x] ++ (tail (snd lines))) 
                                                 | x <- hyphenate map (head (snd lines)), lineLength (fst lines ++ [fst x]) <= a]
              where
                     lines = breakLine a xs

-- incrementNby:
-- Increments the first N elements of a list by K
incrementNby :: Int -> Int -> [Int] -> [Int]
incrementNby n k xs = [x + k| x <- fst (split)] ++ [x | x <- snd (split)] 
              where
                     split = Data.List.splitAt n xs


-- disperse:
-- Distributes N elements into a list of K elements one by one 
disperse :: Int -> [Int] -> [Int]
disperse 0 xs = xs
disperse a xs | a > length xs = disperse (a - length xs) (incrementNby a 1 xs) 
              | a <= length xs = disperse 0 (incrementNby a 1 xs)


-- generateBlanks:
-- Generates blanks that will separate each one of the words
generateBlanks :: Int -> Int -> [Line]
generateBlanks len amount = [ [Blank | y <- [1..x]] | x <- disperse amount ([0 | x <- [1..(len - 1)]]) ]


-- intercalateLines:
-- Intercalates the elements of one Line with list of Lines
intercalateLines :: Line -> [Line] -> Line
intercalateLines xs [] = xs 
intercalateLines (x:xs) (z:zs) = [x] ++ z ++ intercalateLines xs zs


-- insertBlanks:
-- Distribuites a set amount of blanks between the words of a line.
insertBlanks :: Int -> Line -> Line
insertBlanks _ [] = []
insertBlanks _ [x] = [x]
insertBlanks 0 xs = xs
insertBlanks a xs = intercalateLines xs (generateBlanks (length xs) a)


-- TODO separarYalinear:
-- Separates and alligns the text
separarYalinear :: HypMap -> Int -> Flag -> Flag -> String -> [String]

separarYalinear map a NOSEPARAR NOAJUSTAR xs | two == [] = [line2string one]
       | otherwise = [line2string one] ++ separarYalinear map a NOSEPARAR NOAJUSTAR (line2string two)
              where 
                     split = breakLine a (string2line xs)
                     one = fst split
                     two = snd split

separarYalinear map a NOSEPARAR AJUSTAR xs | two == [] = [line2string withBlanks]
       | otherwise = [line2string withBlanks] ++ separarYalinear map a NOSEPARAR AJUSTAR (line2string two)
              where 
                     split = breakLine a (string2line xs)
                     one = fst split
                     two = snd split
                     withBlanks = insertBlanks (a - lineLength one) one

separarYalinear map a SEPARAR NOAJUSTAR xs | two == [] = [line2string one]
       | otherwise = [line2string one] ++ separarYalinear map a SEPARAR NOAJUSTAR (line2string two)
              where
                     split = last (lineBreaks map a (string2line xs))
                     one = fst split
                     two = snd split

separarYalinear map a SEPARAR AJUSTAR xs | two == [] = [line2string one]
       | otherwise = [line2string withBlanks] ++ separarYalinear map a SEPARAR AJUSTAR (line2string two)
              where
                     split = last (lineBreaks map a (string2line xs))
                     one = fst split
                     two = snd split
                     withBlanks = insertBlanks (a - lineLength one) one