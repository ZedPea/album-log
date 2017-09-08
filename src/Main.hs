{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main
(
    main
)
where

import System.IO (stdout, hFlush, openTempFile, hPutStr, hClose)
import System.Directory (doesFileExist, renameFile, removeFile)
import Control.Monad (unless, liftM2)
import Control.Exception (bracket, catch, throwIO)
import System.IO.Error (isDoesNotExistError)
import Data.Char (toLower)
import System.Console.CmdArgs (Data, Typeable, (&=), help, def, typFile,
                               program, cmdArgs)

import Parse (parseFileName)
import Decode (decode)
import Actions (add, createFile, remove)
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
    args <- cmdArgs albumLog

    case command args of
        "add" -> do
            tryParse <- simpleSetup
            case tryParse of
                Left err -> putStrLn err
                Right f -> do
                    (artist', album') <- getArtistAlbum args
                    final <- add f artist' album'
                    writeToDisk final

        "remove" -> do
            tryParse <- simpleSetup
            case tryParse of
                Left err -> putStrLn err
                Right f -> do
                    (artist', album') <- getArtistAlbum args
                    let finalEither = remove f artist' album'
                    case finalEither of
                        Left err -> putStrLn err
                        Right final -> writeToDisk final

        "create" -> writeToDisk createFile

        x -> putStrLn $ "Unknown operation: " ++ x

simpleSetup :: IO (Either String FileInfo)
simpleSetup = do
    args <- cmdArgs albumLog
    filepath <- getFile (file args)
    tryParse <- parseFileName filepath
    
    case tryParse of
        Left err -> return . Left $ "Error parsing file: " ++ show err
        --rewrap either ...
        Right parsed -> return $ Right parsed

getArtistAlbum :: AlbumLog -> IO (String, String)
getArtistAlbum args = liftM2 (\x y -> (x, y)) 
    (map toLower <$> getString (artist args) "Artist name: ")
    (map toLower <$> getString (album args) "Album name: ")
          
getString :: String -> String -> IO String
getString s msg
    | not $ null s = return s
    | otherwise = do
        putStr msg
        hFlush stdout
        getLine

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

-- if the file doesn't exists to begin with, renaming the file will
-- fail, and so the tmp file will never be removed.
-- this could occur because the user deleted it for example

-- so we have to attempt to remove it rather than just removing it
-- regardless, as when everything goes ok, removing a non existant file
-- throws an error
writeToDisk :: FileInfo -> IO ()
writeToDisk f = do

    --args <- cmdArgs albumLog
    
    bracket  (openTempFile "." "album-log.tmp")

             (\(tmpFile, tmpHandle) -> do
                hClose tmpHandle
                removeIfExists tmpFile)

             (\(tmpFile, tmpHandle) -> do
                hPutStr tmpHandle (decode f)
                --in place update
                {-renameFile tmpFile $ if null (file args)
                    then "output.txt"
                    else file args-}

                renameFile tmpFile "output.txt")

    return ()

-- https://stackoverflow.com/a/8502391
removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
    where handleExists e
            | isDoesNotExistError e = return ()
            | otherwise = throwIO e
