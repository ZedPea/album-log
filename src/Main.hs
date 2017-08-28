module Main
(
    main
)
where

import System.Environment (getArgs)

import Parse (parseFileName)

main :: IO ()
main = print =<< parseFileName =<< head <$> getArgs
