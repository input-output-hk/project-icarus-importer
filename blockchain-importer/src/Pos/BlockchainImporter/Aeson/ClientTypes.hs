{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

module Pos.BlockchainImporter.Aeson.ClientTypes
       (
       ) where

import           Universum

import           Data.Aeson.Encoding (unsafeToEncoding)
import           Data.Aeson.TH (defaultOptions, deriveJSON, deriveToJSON)
import           Data.Aeson.Types (FromJSON (..), Parser, ToJSON (..), Value (String), typeMismatch)
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Builder as BS (string8)
import qualified Data.ByteString.Lazy as BSL
import           Data.Fixed (showFixed)
import qualified Data.Text.Lazy.Encoding as TE

import           Pos.Aeson ()
import           Pos.Aeson.Txp ()
import           Pos.BlockchainImporter.Web.ClientTypes (CAda (..), CAddress, CAddressSummary,
                                                         CAddressType, CBlockEntry, CBlockSummary,
                                                         CCoin, CEncodedSTx (..),
                                                         CGenesisAddressInfo, CGenesisSummary,
                                                         CHash, CNetworkAddress, CTxBrief, CTxEntry,
                                                         CTxId, CTxSummary)
import           Pos.BlockchainImporter.Web.Error (BlockchainImporterError)

deriveJSON defaultOptions ''CHash
deriveJSON defaultOptions ''CAddress
deriveJSON defaultOptions ''CTxId
deriveJSON defaultOptions ''CEncodedSTx

deriveToJSON defaultOptions ''CCoin
deriveToJSON defaultOptions ''BlockchainImporterError
deriveToJSON defaultOptions ''CBlockEntry
deriveToJSON defaultOptions ''CTxEntry
deriveToJSON defaultOptions ''CTxBrief
deriveToJSON defaultOptions ''CAddressType
deriveToJSON defaultOptions ''CAddressSummary
deriveToJSON defaultOptions ''CBlockSummary
deriveToJSON defaultOptions ''CNetworkAddress
deriveToJSON defaultOptions ''CTxSummary
deriveToJSON defaultOptions ''CGenesisSummary
deriveToJSON defaultOptions ''CGenesisAddressInfo

instance ToJSON CAda where
    -- https://github.com/bos/aeson/issues/227#issuecomment-245400284
    toEncoding (CAda ada) =
        showFixed True ada & -- convert Micro to String chopping off trailing zeros
        BS.string8 &         -- convert String to ByteString using Latin1 encoding
        unsafeToEncoding     -- convert ByteString to Aeson's Encoding

-- JSON instances for ByteString, it is encoded to base 64
instance ToJSON BSL.ByteString where
  toJSON bs = String $ toStrict $ TE.decodeUtf8 $ B64.encode bs

instance FromJSON BSL.ByteString where
  parseJSON (String encodedData) = fromBase64Data encodedData
  parseJSON x                    = typeMismatch "Not a valid ByteString" x

fromBase64Data :: Text -> Parser BSL.ByteString
fromBase64Data base64EncodedData = do
    let lazyRawEncData    = fromStrict base64EncodedData
        utf8EncRawData    = TE.encodeUtf8 $ lazyRawEncData
        maybeBase64DecRaw = B64.decode $ utf8EncRawData
    case maybeBase64DecRaw of
        Right base64DecRaw -> pure $ base64DecRaw
        Left e             -> fail e
