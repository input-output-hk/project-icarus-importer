{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

module Pos.BlockchainImporter.Aeson.ClientTypes
       (
       ) where

import           Universum

import           Data.Aeson.Encoding (unsafeToEncoding)
import           Data.Aeson.TH (defaultOptions, deriveJSON, deriveToJSON)
import           Data.Aeson.Types (Parser, Value (String), typeMismatch, ToJSON (..), FromJSON (..))
import qualified Data.ByteString.Builder as BS (string8)
import qualified Data.ByteString.Base64.Lazy as B64
import           Data.Fixed (showFixed)
import qualified Data.Text.Lazy.Encoding as TE

import           Pos.Aeson ()
import           Pos.Aeson.Txp ()
import           Pos.BlockchainImporter.Web.ClientTypes (CAda (..), CAddress, CAddressSummary, CAddressType,
                                               CBlockEntry, CBlockSummary, CCoin,
                                               CGenesisAddressInfo, CGenesisSummary, CHash,
                                               CNetworkAddress, CTxBrief, CTxEntry, CTxId,
                                               CTxSummary, CSignedEncTx, CEncodedData (..))
import           Pos.BlockchainImporter.Web.Error (BlockchainImporterError)

deriveJSON defaultOptions ''CHash
deriveJSON defaultOptions ''CAddress
deriveJSON defaultOptions ''CTxId
deriveJSON defaultOptions ''CSignedEncTx

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

instance ToJSON CEncodedData where
    toJSON (CEncodedData bs) = String $ toStrict $ TE.decodeUtf8 $ B64.encode bs
    
instance FromJSON CEncodedData where
    parseJSON (String encodedData) = fromRawEncodedData encodedData
    parseJSON x                    = typeMismatch "Not a valid CEncodedData" x
    
fromRawEncodedData :: Text -> Parser CEncodedData
fromRawEncodedData rawEncodedData = do
    let lazyRawEncData    = fromStrict rawEncodedData
        utf8EncRawData    = TE.encodeUtf8 $ lazyRawEncData
        maybeBase64DecRaw = B64.decode $ utf8EncRawData
    case maybeBase64DecRaw of
        Right base64DecRaw -> pure $ CEncodedData $ base64DecRaw
        Left e             -> fail e
