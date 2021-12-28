{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import BatchProcessor
    ( DataInsertionContext(..), processInsertions, ProcessorContext(..) )
import Database.PostgreSQL.Simple.SqlQQ ( sql )
import Dataset.DailySummaries ( DlyRecord, parseRecord )
import Dataset.Stations ( Station, parseStation )
import OptionsParser ( CDOOptions(..), Dataset(..), runOptionsParser )

dailiesContext :: CDOOptions -> DataInsertionContext DlyRecord
dailiesContext CDOOptions{..} = 
  let query = 
        [sql|
            INSERT INTO daily_records (station_id, date, element, value, mflag, qflag, sflag)
            VALUES (?,?,?,?,?,?,?)
        |]
      insertionFailureFile = cdoLogPath <> "/dailies_insertion_failures"
      parseFailureFile = cdoLogPath <> "/dailies_parse_failures"
      filepath = "data/ghcn/superghcnd/superghcnd_full_20211225.csv"
      parser = parseRecord
      batchSize = cdoBatchSize
  in DataInsertionContext{..}

stationsContext :: CDOOptions -> DataInsertionContext Station
stationsContext CDOOptions{..} =
  let query = 
        [sql|
            INSERT INTO stations (station_id, gps, elevation, state, name, gsn_flag, hcn_crn_flag, wmo_id)
            VALUES (?,?,?,?,?,?,?,?)
        |]
      insertionFailureFile = cdoLogPath <> "/station_insertion_failures"
      parseFailureFile = cdoLogPath <> "/station_parse_failures"
      filepath = "data/ghcn/ghcnd-stations.txt"
      parser = parseStation
      batchSize = cdoBatchSize
  in DataInsertionContext{..}

main :: IO ()
main = runOptionsParser >>= runProcessor

runProcessor :: CDOOptions -> IO ()
runProcessor opts@CDOOptions{..} =
  let batchCtx = ProcessorContext cdoDebugMode
  in case cdoDataset  of
    Dailies -> processInsertions batchCtx (dailiesContext opts) 
    Stations -> processInsertions batchCtx (stationsContext opts) 
