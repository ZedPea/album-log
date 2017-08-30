{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main
(
    main
)
where

import System.IO (stdout, hFlush)
import System.Directory (doesFileExist)
import Control.Monad (unless)
import System.Console.CmdArgs

import Parse (parseFileName)
import Decode (decode)
import Actions (addInteractive, add)
import Utilities (nonExistentMsg)

data AlbumLog = AlbumLog {
    command :: String,
    file :: FilePath,
    artist :: String,
    album :: String
} deriving (Show, Data, Typeable)

albumLog :: AlbumLog
albumLog = AlbumLog {
    command = "add" &= help "Command to execute - add, create, etc",
    file = def &= help "Filepath to load and parse" &= typFile,
    artist = def &= help "Artist to modify",
    album = def &= help "Album to modify"
} &= program "album-log"

main :: IO ()
main = do
    args' <- cmdArgs albumLog
    case command args' of
        "add" -> do
            filepath <- getFile (file args')
            tryParse <- parseFileName filepath

            case tryParse of
                Left err -> putStrLn $ "Error parsing file: " ++ show err
                Right parsed -> do
                    final <- if any null [artist args', album args']
                                then addInteractive parsed
                                else add parsed (artist args') (album args')
                    writeFile "output.txt" (decode final)

        "create" -> return ()

        x -> putStrLn $ "Unknown operation: " ++ x

getFile :: FilePath -> IO FilePath
getFile f = do
    exists <- doesFileExist f
    if exists
        then return f
        else do
            unless (null f) $ putStrLn nonExistentMsg
            putStr "File to open/parse: "
            hFlush stdout
            getFile =<< getLine
