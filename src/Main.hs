module Main
(
    main
)
where

import System.Environment (getArgs)

import Parse (parseFileName)
import Decode (decode)
import Actions (addInteractive)

main :: IO ()
main = do
    file <- head <$> getArgs
    tryParse <- parseFileName file
    case tryParse of
        Left err -> print err
        Right parsed -> do
            final <- addInteractive parsed
            writeFile "output.txt" (decode final)
