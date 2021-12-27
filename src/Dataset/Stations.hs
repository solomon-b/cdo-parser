module Dataset.Stations ( Station(..)
                        , runParseStation
                        , parseStation
                        ) where

import Control.Applicative ( Alternative((<|>), many) )
import Data.Attoparsec.Text
    ( (<?>), double, space, anyChar, parseOnly, Parser )
import Data.Text qualified as T
import Database.PostgreSQL.Simple (ToRow)
import Database.PostgreSQL.Simple.ToField (ToField(..), Action(..))
import GHC.Generics (Generic)

-------------
--- TYPES ---
-------------

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
-- ACW00011604  17.1167  -61.7833   10.1    ST JOHNS COOLIDGE FLD                       
-- ACW00011647  17.1333  -61.7833   19.2    ST JOHNS                                    
-- AE000041196  25.3330   55.5170   34.0    SHARJAH INTER. AIRP            GSN     41196

runParseStation :: T.Text -> Either String Station
runParseStation = parseOnly parseStation

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
