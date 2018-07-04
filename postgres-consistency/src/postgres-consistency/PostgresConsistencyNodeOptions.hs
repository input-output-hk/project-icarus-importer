{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP           #-}
{-# LANGUAGE QuasiQuotes   #-}

-- | Command line options of blockchainImporter node.

module PostgresConsistencyNodeOptions
       ( PostgresConsistencyNodeArgs (..)
       , PostgresConsistencyArgs (..)
       , PostgresChecks (..)
       , getPostgresConsistencyNodeOptions
       ) where

import           Universum

import           Data.Version (showVersion)
import qualified Database.PostgreSQL.Simple as PGS
import           Options.Applicative (Parser, auto, command, execParser, footerDoc, fullDesc,
                                      header, help, helper, info, infoOption, long, metavar, option,
                                      progDesc, showDefault, strOption, subparser, value)

import           Paths_cardano_sl_postgres_consistency (version)
import           Pos.Client.CLI (CommonNodeArgs (..))
import qualified Pos.Client.CLI as CLI


data PostgresConsistencyNodeArgs = PostgresConsistencyNodeArgs
    { enaCommonNodeArgs          :: !CommonNodeArgs
    , enaPostgresConsistencyArgs :: !PostgresConsistencyArgs
    } deriving Show

data PostgresChecks = ExternalConsistencyFromBlk String
                    | RandomExternalConsistency FilePath
                    | InternalConsistency
                    | ExternalTxRangeConsistency String
                    | GetTipHash
                    deriving Show

-- | PostgresConsistency specific arguments.
data PostgresConsistencyArgs = PostgresConsistencyArgs
    { webPort             :: !Word16
    -- ^ The port for the blockchainImporter backend
    , postGresConfig      :: !PGS.ConnectInfo
    -- ^ Configuration of the PostGres DB
    , storingStartBlockPG :: !Word64
    -- ^ Starting block number from which data will be stored on the DB
    , checksToDo          :: !PostgresChecks
    -- ^ File with blk hashes to check for consistency
    } deriving Show

-- Parses the postgres configuration, using the defaults from 'PGS.defaultConnectInfo'
connectInfoParser :: Parser PGS.ConnectInfo
connectInfoParser = do
  connectDatabase <- strOption $
      long    "postgres-name" <>
      metavar "PS-NAME" <>
      value   (PGS.connectDatabase PGS.defaultConnectInfo) <> showDefault <>
      help    "Name of the postgres DB."
  connectUser     <- strOption $
      long    "postgres-user" <>
      metavar "PS-USER" <>
      value   (PGS.connectUser PGS.defaultConnectInfo) <> showDefault <>
      help    "User of the postgres DB."
  connectPassword <- strOption $
      long    "postgres-password" <>
      value   (PGS.connectPassword PGS.defaultConnectInfo) <> showDefault <>
      help    "Password of the postgres DB"
  connectHost     <- strOption $
      long    "postgres-host" <>
      metavar "PS-HOST" <>
      value   (PGS.connectHost PGS.defaultConnectInfo) <> showDefault <>
      help    "Host the postgres DB is running on."
  connectPort     <- option auto $
      long    "postgres-port" <>
      metavar "PS-PORT" <>
      value   (PGS.connectPort PGS.defaultConnectInfo) <> showDefault <>
      help    "Port the postgres DB is listening on."
  pure PGS.ConnectInfo{..}

-- | Ther parser for the blockchainImporter arguments.
blockchainImporterArgsParser :: Parser PostgresConsistencyNodeArgs
blockchainImporterArgsParser = do
    commonNodeArgs <- CLI.commonNodeArgsParser
    webPort        <- CLI.webPortOption 8200 "Port for web API."
    postGresConfig <- connectInfoParser
    storingStartBlockPG     <- option auto $
        long    "postgres-startblock" <>
        metavar "PG-START-NUM" <>
        value   0 <>
        help    "First block whose info will be stored on postgres DB."
    checksToDo <- postgresCheckParser
    pure $ PostgresConsistencyNodeArgs commonNodeArgs PostgresConsistencyArgs{..}

postgresCheckParser :: Parser PostgresChecks
postgresCheckParser = do
  let externalCheckFromBlkCmd = command "ext-const-from-blk"
                                  (info externalCheckFromBlkParser
                                  (progDesc "Check external consistency from a given blk with up-to-date node db"))
      externalCheckRandomCmd = command "ext-const-random"
                                (info externalCheckRandomParser
                                (progDesc "Randomly check external consistency with up-to-date node db"))
      internalCheckCmd = command "int-const"
                          (info (pure InternalConsistency)
                          (progDesc "Check internal consistency with importer db"))
      externalTxRangeCheckCmd = command "ext-range-const"
                                  (info externalRangeTxCheckParser
                                  (progDesc "Check tx range consistency with up-to-date node db"))
      getTipHashCmd = command "get-tip-hash"
                        (info (pure GetTipHash)
                        (progDesc "Print block tip hash"))
  subparser (externalCheckFromBlkCmd
          <> externalCheckRandomCmd
          <> internalCheckCmd
          <> externalTxRangeCheckCmd
          <> getTipHashCmd)
  where externalCheckFromBlkParser = do
          blkToCheck <- strOption $
            long    "starting-block" <>
            metavar "STARTING-BLOCK" <>
            help    "Block from where to start checking for consistency."
          pure $ ExternalConsistencyFromBlk blkToCheck
        externalCheckRandomParser = do
          blksToCheck <- strOption $
            long    "blocks-file" <>
            metavar "CONSISTENCY-BLK-HASHES-FILE" <>
            help    "File with block hashes to check for consistency."
          pure $ RandomExternalConsistency blksToCheck
        externalRangeTxCheckParser = do
          tipHash <- strOption $
            long    "tip-hash" <>
            metavar "TIP-HASH" <>
            help    "Hash of the tip block."
          pure $ ExternalTxRangeConsistency tipHash


-- | The parser for the blockchainImporter.
getPostgresConsistencyNodeOptions :: IO PostgresConsistencyNodeArgs
getPostgresConsistencyNodeOptions = execParser programInfo
  where
    programInfo = info (helper <*> versionOption <*> blockchainImporterArgsParser) $
        fullDesc <> progDesc "Consistency checker of the postgres db generated by blockchain importer."
                 <> header "Postgres DB Consistency Checker."
                 <> footerDoc CLI.usageExample

    versionOption = infoOption
        ("cardano-postgres-consistency-" <> showVersion version)
        (long "version" <> help "Show version.")
