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

import Types

parseFileName :: String -> IO (Either ParseError FileInfo)
parseFileName = parseFromFile parseFile

parse :: String -> Either ParseError FileInfo
parse = P.parse parseFile ""

parseFile :: Parsec String () FileInfo
parseFile = do
    totalListened' <- parseNumListenedTo

    many1 $ noneOf ['\n', '\r']

    endOfLine

    albumsPerDate' <- many parseDateNumPair

    endOfLine

    artists' <- many parseArtist

    eof

    return $ FileInfo totalListened' albumsPerDate' artists'

parseNumListenedTo :: Parsec String () Integer
parseNumListenedTo = do
    string "# TOTAL ALBUMS LISTENED TO - "
    num <- parseNum
    endOfLine
    return num

parseDateNumPair :: Parsec String () (Day, Integer)
parseDateNumPair = try $ do --need input returned to parser if failed
    endOfLine
    string "# "
    date <- parseDate
    string " - "
    num <- parseNum
    return (date, num)

parseDate :: Parsec String () Day
parseDate = f <$> many1 (oneOf "0123456789/")
    where f s = fromMaybe (error "Couldn't parse date") 
                          (parseTimeM False defaultTimeLocale "%e/%m/%0Y" s)

parseArtist :: Parsec String () Artist
parseArtist = do
    endOfLine

    name <- getName True

    string "- "

    num <- parseNum

    endOfLine
    
    albums' <- many1 parseAlbum

    return $ Artist name num albums'

getName :: Bool -> Parsec String () String
getName cut = do
    name' <- (`safeInit` cut) <$> many (noneOf ['-', '\\', '\r', '\n'])
    
    name2 <- optionMaybe $ do
                string "\\-"
                getName cut

    case name2 of
        Nothing -> return name'
        Just name2' -> return $ name' ++ "\\" ++ name2'
    where safeInit [] _ = []
          safeInit x True = init x
          safeInit x _ = x

parseNum :: Parsec String () Integer
parseNum = read <$> many1 digit

parseAlbum :: Parsec String () Album
parseAlbum = do
    void (many1 $ char ' ') <|> void tab

    album <- getName False

    date <- optionMaybe $ do
        string "- "
        parseDate

    endOfLine

    return $ Album album date
