module Main
(
    main
)
where

import System.Environment (getArgs)

import Parse (parseFileName)
import Decode (decode)

main :: IO ()
main = do
    file <- head <$> getArgs
    tryParse <- parseFileName file
    case tryParse of
        Left err -> print err
        Right parsed -> writeFile "output.txt" (decode parsed)
