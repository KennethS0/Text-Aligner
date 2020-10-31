-- Kenneth Sánchez Ocampo

import System.IO
import TextAlligner
import Data.Map

type Estado = Map [Char] Int

main :: IO ()
main = do
    putStrLn "Available commands:"
    mainLoop (fromList[])

-- 
mainLoop :: Estado -> IO()
mainLoop estado = do
    putStr ">> "
    inputStr <- getLine
    let tokens = words inputStr
    let command = tokens!!0

    case command of
    
        -- TODO load:


        -- TODO show:


        -- TODO ins:


        -- TODO save:


        -- TODO split: 


        -- TODO splitf:


        -- exit:
        -- Ends the cycle
        "exit" -> do
            putStrLn "Goodbye."
