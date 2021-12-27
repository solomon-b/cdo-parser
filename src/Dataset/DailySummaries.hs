module Dataset.DailySummaries ( DlyRecord(..)
                              , parseRecord
                              , runParseRecord
                              ) where

import Control.Applicative ( Alternative((<|>)), optional )
import Control.Monad ( void )
import Data.Attoparsec.Text
    ( many1,
      decimal,
      digit,
      letter,
      signed,
      char,
      parseOnly,
      Parser )
import Data.Text qualified as T
import Data.Time.Calendar ( Day, fromGregorian )
import Database.PostgreSQL.Simple (ToRow, FromRow)
import GHC.Generics (Generic)

-------------
--- TYPES ---
-------------

data DlyRecord = DlyRecord
  { stationId :: T.Text
  , date :: Day
  , element :: T.Text -- measurement type
  , value :: Integer
  , mflag :: Maybe T.Text
  , qflag :: Maybe T.Text
  , sflag :: Maybe T.Text
  } deriving (Show, Eq, Generic, ToRow, FromRow)

--------------
--- PARSER ---
--------------
-- ACW00011604,19490101,TMAX,289,,,X,

runParseRecord :: T.Text -> Either String DlyRecord
runParseRecord = parseOnly parseRecord

parseRecord :: Parser DlyRecord
parseRecord = do
  stationId <- alphaNums
  comma
  date <- parseDate
  comma
  element <- alphaNums
  comma
  value <- signed decimal
  comma
  mflag <- optional $ fmap T.singleton $ digit <|> letter
  comma
  qflag <- optional $ fmap T.singleton $ digit <|> letter
  comma
  sflag <- optional $ fmap T.singleton $ digit <|> letter
  comma
  pure $ DlyRecord stationId date element value mflag qflag sflag

parseDate :: Parser Day
parseDate = do
   year <- fixedWidthInt 4
   month <- fixedWidthInt 2
   day <- fixedWidthInt 2
   pure $ fromGregorian (toInteger year) month day

comma :: Parser ()
comma = void $ char ','

fixedWidthInt :: Int -> Parser Int
fixedWidthInt n = fmap read $ fixedWidth n digit

fixedWidth :: Int -> Parser a -> Parser [a]
fixedWidth n p = sequence $ replicate n p

alphaNums :: Parser T.Text
alphaNums = fmap T.pack $ many1 (letter <|> digit)
