module Utilities
(
    addDash,
    addEscape,
    nonExistentMsg
)
where

import Data.List (intercalate, isPrefixOf)

addDash :: String -> String
addDash = replace "\\" "\\-"

addEscape :: String -> String
addEscape = replace "-" "\\"

nonExistentMsg :: String
nonExistentMsg = "The specified file doesn't exist or you don't have the \
                 \the permissiong to open it."

--the below functions are taken from MissingH/Data.List.Utils
--didn't want to have a large dependency for one function
split :: Eq a => [a] -> [a] -> [[a]]
split _ [] = []
split delim str
    | null remainder = [firstline]
    | remainder == delim = [firstline, []]
    | otherwise = firstline : split delim (drop (length delim) remainder)
    where (firstline, remainder) = breakList (isPrefixOf delim) str

breakList :: ([a] -> Bool) -> [a] -> ([a], [a])
breakList func = spanList (not . func)

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace old new = intercalate new . split old

spanList :: ([a] -> Bool) -> [a] -> ([a], [a])
spanList _ [] = ([],[])
spanList func list@(x:xs)
    | func list = (x:ys, zs)
    | otherwise = ([], list)
    where (ys,zs) = spanList func xs
