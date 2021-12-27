{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Dataset.Superghcnd  where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text
import Data.Foldable
import Data.Bifunctor (first)
import qualified Data.Text as T
import Database.PostgreSQL.Simple (ToRow)
import Data.Time.Calendar
import GHC.Generics (Generic)

-------------
--- TYPES ---
-------------

-- ACW00011604,19490101,TMAX,289,,,X,
data DlyRecord = DlyRecord
  { stationId :: T.Text
  , date :: Day
  , element :: T.Text
  , value :: Integer
  , mflag :: Maybe T.Text
  , qflag :: Maybe T.Text
  , sflag :: Maybe T.Text
  } deriving (Show, Eq, Generic, ToRow)

--------------
--- PARSER ---
--------------

runParseRecords = parseOnly parseRecords

parseRecords :: Parser [DlyRecord]
parseRecords = parseRecord `sepBy` char '\n'

parseRecord :: Parser DlyRecord
parseRecord = do
  stationId <- alphaNums
  comma
  date <- parseDate
  comma
  element <- alphaNums
  comma
  value <- fmap read $ many1 digit
  comma
  mflag <- optional $ fmap T.singleton $ digit <|> letter
  comma
  qflag <- optional $ fmap T.singleton $ digit <|> letter
  comma
  sflag <- optional $ fmap T.singleton $ digit <|> letter
  comma
  endOfLine
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
