{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP           #-}
{-# LANGUAGE QuasiQuotes   #-}

-- | Command line options of blockchainImporter node.

module BlockchainImporterNodeOptions
       ( BlockchainImporterNodeArgs (..)
       , BlockchainImporterArgs (..)
       , getBlockchainImporterNodeOptions
       ) where

import           Universum

import           Data.Version (showVersion)
import qualified Database.PostgreSQL.Simple as PGS
import           Options.Applicative (Parser, auto, execParser, footerDoc, fullDesc, header, help,
                                      helper, info, infoOption, long, metavar, option, progDesc,
                                      showDefault, strOption, value)

import           Paths_cardano_sl_blockchain_importer (version)
import           Pos.Client.CLI (CommonNodeArgs (..))
import qualified Pos.Client.CLI as CLI


data BlockchainImporterNodeArgs = BlockchainImporterNodeArgs
    { enaCommonNodeArgs         :: !CommonNodeArgs
    , enaBlockchainImporterArgs :: !BlockchainImporterArgs
    } deriving Show

-- | BlockchainImporter specific arguments.
data BlockchainImporterArgs = BlockchainImporterArgs
    { webPort        :: !Word16
    -- ^ The port for the blockchainImporter backend
    , postGresConfig :: !PGS.ConnectInfo
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
blockchainImporterArgsParser :: Parser BlockchainImporterNodeArgs
blockchainImporterArgsParser = do
    commonNodeArgs <- CLI.commonNodeArgsParser
    webPort        <- CLI.webPortOption 8200 "Port for web API."
    postGresConfig <- connectInfoParser
    pure $ BlockchainImporterNodeArgs commonNodeArgs BlockchainImporterArgs{..}

-- | The parser for the blockchainImporter.
getBlockchainImporterNodeOptions :: IO BlockchainImporterNodeArgs
getBlockchainImporterNodeOptions = execParser programInfo
  where
    programInfo = info (helper <*> versionOption <*> blockchainImporterArgsParser) $
        fullDesc <> progDesc "Cardano SL main server node w/ blockchainImporter."
                 <> header "Cardano SL blockchainImporter."
                 <> footerDoc CLI.usageExample

    versionOption = infoOption
        ("cardano-blockchain-importer-" <> showVersion version)
        (long "version" <> help "Show version.")
