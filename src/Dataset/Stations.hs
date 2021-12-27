{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Dataset.Stations where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text
import Data.Foldable
import Data.Bifunctor (first)
import qualified Data.Text as T
import qualified Data.Scientific as S
import Database.PostgreSQL.Simple (ToRow)
import Database.PostgreSQL.Simple.ToField
import Data.Time.Calendar
import GHC.Generics (Generic)

-------------
--- TYPES ---
-------------
-- ACW00011604  17.1167  -61.7833   10.1    ST JOHNS COOLIDGE FLD                       
-- ACW00011647  17.1333  -61.7833   19.2    ST JOHNS                                    
-- AE000041196  25.3330   55.5170   34.0    SHARJAH INTER. AIRP            GSN     41196

data Point = Point Double Double
  deriving (Show, Eq, Generic)

instance ToField Point where
  toField (Point lat long) = Many [Plain "POINT(", toField lat, Plain ", ",  toField long, Plain ")"]
    
data Station = Station
  { stationId :: T.Text
  , gps :: Point
  , elevation :: Double
  , state :: Maybe T.Text
  , name :: T.Text
  , gsnFlag :: Maybe T.Text
  , hcnCrnFlag :: Maybe T.Text
  , wmoId :: Maybe T.Text
  } deriving (Show, Eq, Generic, ToRow)
  
--------------
--- PARSER ---
--------------

runParseStations = parseOnly parseStations
runParseStation = parseOnly parseStation

parseStations :: Parser [Station]
parseStations = parseStation `sepBy` char '\n'
-- AF000040930  35.3170   69.0170 3366.0    NORTH-SALANG                   GSN     40930

parseStation :: Parser Station
parseStation = do
  stationId <- fixedWidthWord 11 <?> "stationId"
  space <?> "after stationId"
  lat <- many space *> double <?> "lat"
  space <?> "afer lat"
  long <- many space *> double <?> "long"
  space <?> "after long"
  elevation <- many space *> double <?> "elevation"
  space <?> "after elevation"
  state <- fixedWidthWordOpt 2 
  space
  name <- fmap T.strip $ fixedWidthWord 30
  space
  gsn <- fixedWidthWordOpt 3
  space
  hcnCrn <- fixedWidthWordOpt 3
  space
  wmo <- fixedWidthWordOpt 5
  pure $ Station stationId (Point lat long) elevation state name gsn hcnCrn wmo

fixedWidthInt :: Int -> Parser Int
fixedWidthInt n = fmap read $ fixedWidth n digit

fixedWidthWord :: Int -> Parser T.Text
fixedWidthWord n = fmap T.pack (fixedWidth n anyChar)

fixedWidthWordOpt :: Int -> Parser (Maybe T.Text)
fixedWidthWordOpt n = (fmap . fmap) T.pack (fixedWidthOpt n anyChar)

fixedWidth :: Int -> Parser a -> Parser [a]
fixedWidth n p = sequence $ replicate n p

fixedWidthOpt :: Int -> Parser a -> Parser (Maybe [a])
fixedWidthOpt n p' =
  let np = Nothing <$ fixedWidth n space
      p = fmap Just $ fixedWidth n p'
  in np <|> p
