{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main
(
    main
)
where

import System.IO (stdout, hFlush, openTempFile, hPutStr, hClose)
import System.Directory (doesFileExist, renameFile, removeFile)
import Control.Monad (unless)
import Data.Maybe (fromMaybe)
import Control.Exception (bracket, catch, throwIO)
import System.IO.Error (isDoesNotExistError)
import System.Console.CmdArgs

import Parse (parseFileName)
import Decode (decode)
import Actions (addInteractive, add)
import Utilities (nonExistentMsg)
import Types

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

                    writeToDisk Nothing final

                    {- uncomment in release verion
                    writeToDisk (if exists 
                                    then Just (file args') 
                                    else Nothing) new
                    -}

        "create" -> do
            let new = FileInfo [] 0 [] []

            writeToDisk (if null $ file args' -- this can fail if file can't
                            then Nothing      -- be created
                            else Just $ file args') new

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

writeToDisk :: Maybe FilePath -> FileInfo -> IO ()
writeToDisk filepath f = bracket
    (openTempFile "." "album-log.tmp")

    -- if the file doesn't exists to begin with, renaming the file will
    -- fail, and so the tmp file will never be removed.
    -- we have to attempt to remove it rather than just removing it
    -- regardless, as when everything goes ok, removing a non existant file
    -- throws an error
    (\(tmpFile, tmpHandle) -> removeIfExists tmpFile >> hClose tmpHandle)

    (\(tmpFile, tmpHandle) -> do

        hPutStr tmpHandle (decode f)
        renameFile tmpFile $ fromMaybe "output.txt" filepath)

-- https://stackoverflow.com/a/8502391
removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
    where handleExists e
            | isDoesNotExistError e = return ()
            | otherwise = throwIO e
