{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import BatchProcessor
    ( DataInsertionContext(..), processInsertions )
import Database.PostgreSQL.Simple.SqlQQ ( sql )
import Dataset.DailySummaries ( DlyRecord, parseRecord )
import Dataset.Stations ( Station, parseStation )

dailiesContext :: DataInsertionContext DlyRecord
dailiesContext = 
  let query = 
        [sql|
            INSERT INTO daily_records (station_id, date, element, value, mflag, qflag, sflag)
            VALUES (?,?,?,?,?,?,?)
        |]
      insertionFailureFile = "/tmp/dailies_insertion_failures"
      parseFailureFile = "/tmp/dailies_parse_failures"
      filepath = "data/ghcn/superghcnd/superghcnd_full_20211225.csv"
      parser = parseRecord
  in DataInsertionContext{..}

stationsContext :: DataInsertionContext Station
stationsContext =
  let query = 
        [sql|
            INSERT INTO stations (station_id, gps, elevation, state, name, gsn_flag, hcn_crn_flag, wmo_id)
            VALUES (?,?,?,?,?,?,?,?)
        |]
      insertionFailureFile = "/tmp/station_insertion_failures"
      parseFailureFile = "/tmp/station_parse_failures"
      filepath = "data/ghcn/ghcnd-stations.txt"
      parser = parseStation
  in DataInsertionContext{..}

main :: IO ()
main = do
  processInsertions dailiesContext 
  --processInsertions stationsContext 
