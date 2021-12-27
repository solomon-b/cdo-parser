module BatchProcessor ( DataInsertionContext(..)
                      , insertBatch
                      , processInsertions
                      ) where

import Control.Exception ( try )
import Control.Monad ( when )
import Data.Attoparsec.Text qualified as Attoparsec
import Data.Either ( partitionEithers )
import Data.Function ( (&) )
import Data.Foldable ( traverse_ )
import Data.Text qualified as T
import Database.PostgreSQL.Simple qualified as DPS
import Streamly.Data.Fold qualified as Fold
import Streamly.FileSystem.Handle qualified as Handle
import Streamly.Internal.Unicode.Stream qualified as Unicode
import Streamly.Prelude qualified as Stream
import System.IO ( IOMode(..), openFile )

write :: Monad m => Fold.Fold m Char T.Text
write = Fold.foldl' (T.snoc) mempty

distribute :: (x, Either a b) -> Either (x, a) (x, b)
distribute (x, Left a) = Left (x, a)
distribute (x, Right b) = Right (x, b)

data DataInsertionContext a = DataInsertionContext
  { query :: DPS.Query
  , insertionFailureFile :: FilePath
  , parseFailureFile :: FilePath
  , filepath :: FilePath
  , parser :: Attoparsec.Parser a
  }

insertBatch :: DPS.ToRow a => DataInsertionContext a -> DPS.Connection -> [(Int, Either String a)] -> IO ()
insertBatch DataInsertionContext{..} conn xs = xs & fmap distribute & partitionEithers & \case
  (errs, batch) -> do
    when (not (null errs)) $ do
      putStrLn $ "The following rows failed to parse:"
      traverse_ (appendFile parseFailureFile . flip mappend "\n" . show) errs
      print errs
    let rowNums = fmap fst xs
        batchRange = show (minimum rowNums) <> " - " <> show (maximum rowNums)
    putStrLn $ "Attempting to insert successfully parsed records from batch " <> batchRange

    result <- try $ flip (DPS.executeMany conn) (fmap snd batch) query
    case result of
      Left (e :: DPS.SqlError) -> do
        putStrLn $ "FAILURES OCCURED IN BATCH " <> batchRange
        print e
        appendFile insertionFailureFile (batchRange <> "\n")
      Right _ -> pure ()
 
processInsertions :: DPS.ToRow a => DataInsertionContext a -> IO ()
processInsertions ctx@DataInsertionContext{..} = do
  conn <- DPS.connectPostgreSQL "postgres://postgres:password@localhost/ghcnd"
  handle <- openFile filepath ReadMode

  writeFile parseFailureFile ""
  writeFile insertionFailureFile ""

  Stream.unfold Handle.read handle
      & Unicode.decodeUtf8
      & Unicode.lines write
      -- & Stream.take 2000
      & Stream.map (Attoparsec.parseOnly parser)
      & Stream.zipWith (,) (Stream.enumerateFrom 0)
      & Stream.chunksOf 1000 Fold.toList
      & Stream.mapM (insertBatch ctx conn)
      & Stream.drain