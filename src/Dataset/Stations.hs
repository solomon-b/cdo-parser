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
import Data.Time.Calendar
import GHC.Generics (Generic)

-------------
--- TYPES ---
-------------
-- ACW00011604  17.1167  -61.7833   10.1    ST JOHNS COOLIDGE FLD                       
-- ACW00011647  17.1333  -61.7833   19.2    ST JOHNS                                    
-- AE000041196  25.3330   55.5170   34.0    SHARJAH INTER. AIRP            GSN     41196

data Station = Station
  { stationId :: T.Text
  , gps :: (S.Scientific, S.Scientific)
  , elevation :: S.Scientific
  , state :: Maybe T.Text
  , name :: T.Text
  , gsnFlag :: Maybe T.Text
  , hcnCrnFlag :: Maybe T.Text
  , wmoId :: Maybe T.Text
  } deriving (Show, Eq, Generic)
  
--------------
--- PARSER ---
--------------

runParseStations = parseOnly parseStations

parseStations :: Parser [Station]
parseStations = parseStation `sepBy` char '\n'

parseStation :: Parser Station
parseStation = do
  stationId <- fixedWidthWord 11
  space
  lat <- some space *> scientific
  space
  long <- some space *> scientific
  space
  elevation <- some space *> scientific
  space
  state <- fixedWidthWordOpt 2
  space
  name <- fmap T.strip $ fixedWidthWord 30
  space
  gsn <- fixedWidthWordOpt 3
  space
  hcnCrn <- fixedWidthWordOpt 3
  space
  wmo <- fixedWidthWordOpt 5
  endOfLine
  pure $ Station stationId (lat, long) elevation state name gsn hcnCrn wmo

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
