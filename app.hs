-- Kenneth Sánchez Ocampo

import System.IO
import TextAlligner
import Data.Map
import Data.List

type Estado = Map String State

main :: IO ()
main = do
    putStrLn "Available commands:"
    putStrLn "-> load (file)"
    putStrLn "-> show"
    putStrLn "-> ins (word) (wo-rd)"
    putStrLn "-> save (file name)"
    putStrLn "-> split (length) (s/n) (s/n) (text)"
    putStrLn "-> splitf (length) (s/n) (s/n) (file) (optionalSaveFile)"
    putStrLn "-> exit"
    mainLoop (fromList [])


mainLoop :: HypMap -> IO()
mainLoop dictionary = do
    putStr ">> "
    inputStr <- getLine
    let tokens = words inputStr ++ ["FLAG404"]
    let command = tokens!!0

    case command of
        -- load:
        -- Loads a dictionary of words with is syllable separation
        "load" -> do
            datos <- readFile (tokens!!1)
            let x = words datos
            let dic = Data.Map.fromList [((x!!i), separateBy '-' (x!!(i + 1)))| i <- [0..length x - 1], even i]
            putStrLn ("Dictionary loaded succesfully. Words loaded: " ++ (show (length (Data.Map.keys dic))))
            mainLoop (Data.Map.union dic dictionary)

        -- show:
        -- Shows the loaded dictionary
        "show" -> do
            putStrLn "DATA LOADED:"
            putStrLn (Data.List.drop 9 (show dictionary))
            mainLoop (dictionary)

        -- ins:
        -- Adds a new word with its separataion (inserted by the user) into the dictionary
        "ins" -> do
            let word = (tokens!!1)
            let separation = (tokens!!2)
            putStrLn "Inserting..."
            mainLoop ( Data.Map.insert word (separateBy '-' separation) dictionary )

        -- save:
        -- Saves the loaded dictionary into a file.
        "save" -> do
            putStrLn "Saving..."
            let datos = assocs (dictionary)
            writeFile (tokens!!1) (concat [ (fst x) ++ " " ++ Data.List.intercalate "-" (snd x) ++ "\n" | x <- datos])
            putStrLn ("Total of word saved: " ++ show (length datos))
            mainLoop dictionary 

        -- split: 
        -- Separates text given by the user and shows it for the user. 
        "split" -> do
            let m1 = Data.Map.fromList([("s", AJUSTAR), ("n", NOAJUSTAR)])
            let m2 = Data.Map.fromList([("s", SEPARAR), ("n", NOSEPARAR)])

            let allign = (remMaybe (Data.Map.lookup (tokens!!2) m2))!!0
            let separate = (remMaybe (Data.Map.lookup (tokens!!3) m1))!!0

            let inputText = Data.List.intercalate " " (Data.List.drop 4 tokens)
            let outputList = init (TextAlligner.separarYalinear dictionary (read (tokens!!1) :: Int) allign separate inputText)
            putStrLn (Data.List.intercalate "\n" outputList) 
            mainLoop dictionary
 
        -- splitf:
        -- Separates text inside of a file and saves it in another one (optional)
        "splitf" -> do
            let m1 = Data.Map.fromList([("s", AJUSTAR), ("n", NOAJUSTAR)])
            let m2 = Data.Map.fromList([("s", SEPARAR), ("n", NOSEPARAR)])

            let allign = (remMaybe (Data.Map.lookup (tokens!!2) m2))!!0
            let separate = (remMaybe (Data.Map.lookup (tokens!!3) m1))!!0

            inputText <- readFile (tokens!!4)
            let outputList = TextAlligner.separarYalinear dictionary (read (tokens!!1) :: Int) allign separate inputText
            let message = Data.List.intercalate "\n" outputList

            putStrLn message

            case (tokens!!5) of

                "FLAG404" -> do
                    mainLoop dictionary        

                _ -> do
                    writeFile (tokens!!5) message
                    mainLoop dictionary

        -- exit:
        -- Ends the cycle
        "exit" -> do
            putStrLn "Goodbye."

        _ -> do
            putStrLn "Unkown command..."
            mainLoop dictionary
    

-- separateBy:
-- Obtains the syllabic separation of a string "a-b-c" -> ["a", "b", "c"]
separateBy :: Eq a => a -> [a] -> [[a]]
separateBy chr = unfoldr sep where
    sep [] = Nothing
    sep l  = Just . fmap (Data.List.drop 1) . break (== chr) $ l

remMaybe :: Maybe State -> [State]
remMaybe (Just a) = [a]
remMaybe Nothing = []