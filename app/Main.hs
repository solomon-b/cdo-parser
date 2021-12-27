{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Attoparsec.Text (Parser, parseOnly)
import Data.Either
import Data.Function
import Data.Foldable
import Data.List
import Data.Biapplicative
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ
import Dataset.Superghcnd
import Dataset.Stations
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class

import System.IO
import qualified Streamly.Prelude as Stream
import qualified Streamly.FileSystem.Handle as Handle
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.Data.Unfold as Unfold
import qualified Streamly.Unicode.Stream as Unicode
import qualified Streamly.Internal.Unicode.Stream as Unicode

write :: Monad m => Fold.Fold m Char T.Text
write = Fold.foldl' (T.snoc) mempty

distribute :: (x, Either a b) -> Either (x, a) (x, b)
distribute (x, Left a) = Left (x, a)
distribute (x, Right b) = Right (x, b)

data DataInsertionContext a = DataInsertionContext
  { query :: Query
  , insertionFailureFile :: FilePath
  , parseFailureFile :: FilePath
  , filepath :: FilePath
  , parser :: Parser a
  }

insertBatch :: ToRow a => DataInsertionContext a -> Connection -> [(Int, Either String a)] -> IO ()
insertBatch DataInsertionContext{..} conn xs = xs & fmap distribute & partitionEithers & \case
  (errs, batch) -> do
    when (not (null errs)) $ do
      putStrLn $ "The following rows failed to parse:"
      traverse_ (appendFile parseFailureFile . flip mappend "\n" . show) errs
      print errs
    let rowNums = fmap fst xs
        batchRange = show (minimum rowNums) <> " - " <> show (maximum rowNums)
    putStrLn $ "Attempting to insert successfully parsed records from batch " <> batchRange

    result <- try $ flip (executeMany conn) (fmap snd batch) query
    case result of
      Left (e :: SqlError) -> do
        putStrLn $ "FAILURES OCCURED IN BATCH " <> batchRange
        print e
        appendFile insertionFailureFile (batchRange <> "\n")
      Right _ -> pure ()
 
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

processInsertions :: ToRow a => DataInsertionContext a -> IO ()
processInsertions ctx@DataInsertionContext{..} = do
  conn <- connectPostgreSQL "postgres://postgres:password@localhost/ghcnd"
  handle <- openFile filepath ReadMode

  writeFile parseFailureFile ""
  writeFile insertionFailureFile ""

  Stream.unfold Handle.read handle
      & Unicode.decodeUtf8
      & Unicode.lines write
      -- & Stream.take 2000
      & Stream.map (parseOnly parser)
      & Stream.zipWith (,) (Stream.enumerateFrom 0)
      & Stream.chunksOf 1000 Fold.toList
      & Stream.mapM (insertBatch ctx conn)
      & Stream.drain

main :: IO ()
main = do
  processInsertions dailiesContext 
  --processInsertions stationsContext 
