{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Foldable
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class

import Dataset.Superghcnd

hoistMaybe :: (Applicative m) => Maybe b -> MaybeT m b
hoistMaybe = MaybeT . pure

main :: IO ()
main = do
  --file <- TIO.readFile "./data/precipitation_hourly/01/3240_01_1948-1998/3240_014193_por-1998"
  --forM_ (runParseRecords file) $ \record ->
  --  print $ nub $ fmap stationID record
  let record = "ACW00011604,19490101,TMAX,289,,,X,\n"
  case runParseRecords record of
    Left err -> print err
    Right res -> do
      print res
      conn <- connectPostgreSQL "postgres://postgres:password@localhost/ghcnd"
      print "executing insert"
      executeMany conn [sql|
          INSERT INTO daily_records (id, station_id, date, element, value, mflag, qflag, sflag)
          VALUES (nextval('daily_records_id_sequence'),?,?,?,?,?,?,?)
       |] res
      print "executing select"
      mapM_ print =<< ( query_ conn "select * from daily_records" :: IO [Only Int] )
