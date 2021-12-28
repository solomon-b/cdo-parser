module OptionsParser ( CDOOptions(..), Dataset(..), runOptionsParser ) where

import Options.Applicative qualified as Opt

data Dataset = Dailies | Stations --  | GlobalMonth | GlobalYear | ...
  deriving (Show)

data CDOOptions = CDOOptions
  { cdoDataset :: Dataset
  , cdoFilePath :: FilePath
  , cdoLogPath :: FilePath 
    -- ^ Defaults to /tmp
  , cdoBatchSize :: Int
    -- ^ Defaults to 2000
  , cdoDebugMode :: Bool
    -- ^ Defaults to False
  } deriving (Show)

runOptionsParser :: IO CDOOptions
runOptionsParser =
  let opts = Opt.info (parseCDOOptions Opt.<**> Opt.helper) $
        Opt.fullDesc
          <> Opt.progDesc "Process CDO Datasets and load into Postgres"
          <> Opt.header "CDO Parser"
  in Opt.execParser opts

parseCDOOptions :: Opt.Parser CDOOptions
parseCDOOptions =
  CDOOptions
    <$> parseDataset
    <*> parseFilePath
    <*> parseLogPath
    <*> parseBatchSize
    <*> parseDebugMode

readMDataset :: Opt.ReadM Dataset
readMDataset = Opt.eitherReader $ \case
  "dailies" -> pure Dailies
  "stations" -> pure Stations
  err -> Left $ "'" <> err <> "' is not a valid dataset."

parseDataset :: Opt.Parser Dataset
parseDataset =
  let validOptions = "- stations\\n- dailies"
      helpMsg = "Specify the dataset you wish to process.\\nValid Options:\\n" <> validOptions
  in Opt.option readMDataset
    ( Opt.long "dataset-name" <> Opt.short 'n' <> Opt.metavar "FILE_NAME"
        <> Opt.help helpMsg
    )

parseFilePath :: Opt.Parser FilePath
parseFilePath =
  Opt.strOption
    ( Opt.long "data-source" <> Opt.short 'd' <> Opt.metavar "FILE_PATH"
        <> Opt.help "The data file to process"
    )

parseLogPath :: Opt.Parser FilePath
parseLogPath =
  Opt.strOption
    ( Opt.long "log-path" <> Opt.short 'l' <> Opt.metavar "FILE_PATH"
        <> Opt.value "/tmp"
        <> Opt.help "PATH to store log files. Defaults to '/tmp'"
    )

parseBatchSize :: Opt.Parser Int
parseBatchSize =
  Opt.option Opt.auto
    ( Opt.long "batch-size" <> Opt.short 'b' <> Opt.metavar "INTEGER"
        <> Opt.value 2000
        <> Opt.help "Batch Size for writing entries to DB. Defaults to 2000"
    )

parseDebugMode :: Opt.Parser Bool
parseDebugMode =
  Opt.option Opt.auto
    ( Opt.long "debug-mode" <> Opt.short 't' <> Opt.metavar "BOOLEAN"
        <> Opt.value False
        <> Opt.help "Run processor in debug mode where it only processes a single batch"
    )
