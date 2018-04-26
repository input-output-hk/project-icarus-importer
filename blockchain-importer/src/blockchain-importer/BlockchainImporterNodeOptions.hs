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
import           Options.Applicative (Parser, auto, execParser, footerDoc, fullDesc, header, help,
                                      helper, info, infoOption, long, metavar, option, progDesc,
                                      showDefault, value)

import           Paths_cardano_sl_blockchain_importer (version)
import           Pos.Client.CLI (CommonNodeArgs (..))
import qualified Pos.Client.CLI as CLI


data BlockchainImporterNodeArgs = BlockchainImporterNodeArgs
    { enaCommonNodeArgs :: !CommonNodeArgs
    , enaBlockchainImporterArgs   :: !BlockchainImporterArgs
    } deriving Show

-- | BlockchainImporter specific arguments.
data BlockchainImporterArgs = BlockchainImporterArgs
    { webPort      :: !Word16
    -- ^ The port for the blockchainImporter backend
    , notifierPort :: !Word16
    -- ^ The port for the socket.io backend
    } deriving Show

-- | Ther parser for the blockchainImporter arguments.
blockchainImporterArgsParser :: Parser BlockchainImporterNodeArgs
blockchainImporterArgsParser = do
    commonNodeArgs <- CLI.commonNodeArgsParser
    webPort        <- CLI.webPortOption 8100 "Port for web API."
    notifierPort   <- option auto $
        long    "notifier-port" <>
        metavar "PORT" <>
        value   8110 <> showDefault <>
        help    "Port for update notifier, the socket.io backend."

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
