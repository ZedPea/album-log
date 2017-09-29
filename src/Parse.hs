module Parse
(
    parse,
    parseFileName
)
where


import Text.Parsec (Parsec, ParseError, string, digit, many1, oneOf, endOfLine,
                    tab, many, noneOf, try, (<|>), eof, char, optionMaybe)
import Text.Parsec.String (parseFromFile)
import qualified Text.Parsec as P (parse)

import Data.Time (Day(..), parseTimeM, defaultTimeLocale)
import Data.Maybe (fromMaybe)
import Control.Monad (void)

import Types (FileInfo(..), Artist(..), Album(..))
import Sort (sortFileInfoPure)

parseFileName :: String -> IO (Either ParseError FileInfo)
parseFileName s = fmap sortFileInfoPure <$> parseFromFile parseFile s

parse :: String -> Either ParseError FileInfo
parse = fmap sortFileInfoPure <$> P.parse parseFile ""

parseFile :: Parsec String () FileInfo
parseFile = do
    totalListened' <- parseNumListenedTo

    comments' <- many parseComments

    endOfLine

    albumsPerDate' <- many parseDateNumPair

    endOfLine

    optionMaybe endOfLine -- these two option maybes are so blank created files
                          -- parse correctly

    artists' <- many parseArtist

    optionMaybe eof

    return $ FileInfo comments' totalListened' albumsPerDate' artists'

parseComments :: Parsec String () String
parseComments = do
    char '#'
    msg <- many1 $ noneOf ['\n', '\r']
    endOfLine
    return msg

parseNumListenedTo :: Parsec String () Integer
parseNumListenedTo = do
    string "# TOTAL ALBUMS LISTENED TO - "
    num <- parseNum
    endOfLine
    return num

parseDateNumPair :: Parsec String () (Day, Integer)
parseDateNumPair = try $ do --need input returned to parser if failed
    string "# "
    date <- parseDate
    string " - "
    num <- parseNum
    endOfLine
    return (date, num)

parseDate :: Parsec String () Day
parseDate = f <$> many1 (oneOf "0123456789/")
    where f s = fromMaybe (error "Couldn't parse date") 
                          (parseTimeM False defaultTimeLocale "%e/%m/%0Y" s)

parseArtist :: Parsec String () Artist
parseArtist = do
    name <- getName True

    string "- "

    num <- parseNum

    endOfLine
    
    albums' <- many1 parseAlbum

    void endOfLine <|> eof

    return $ Artist name num albums'

getName :: Bool -> Parsec String () String
getName cut = do
    name' <- many (noneOf ['-', '\\', '\r', '\n'])
    
    name2 <- optionMaybe $ do
        string "\\-"
        getName cut

    case name2 of
        Nothing -> return $ safeInit name' cut
        Just name2' -> return $ name' ++ "\\" ++ name2'

    where safeInit [] _ = []
          safeInit x True = init x
          safeInit x _ = x

parseNum :: Parsec String () Integer
parseNum = read <$> many1 digit

parseAlbum :: Parsec String () Album
parseAlbum = do
    void (many1 $ char ' ') <|> void tab

    album <- trim <$> getName False

    date <- optionMaybe $ do
        string "- "
        parseDate

    endOfLine

    return $ Album album date
    where trim xs
            | null xs = []
            | last xs == ' ' = init xs
            | otherwise = xs
