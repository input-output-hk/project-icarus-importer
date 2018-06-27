{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

module Pos.PostgresConsistency.Aeson.ClientTypes
       (
       ) where

import           Universum

import           Data.Aeson.TH (defaultOptions, deriveToJSON)
import           Data.Aeson.Types (FromJSON (..), Parser, Value (String), typeMismatch, withObject,
                                   (.:))
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Lazy.Encoding as TE

import           Pos.Aeson ()
import           Pos.Aeson.Txp ()
import           Pos.PostgresConsistency.Web.ClientTypes (CEncodedSTx (..))
import           Pos.PostgresConsistency.Web.Error (BlockchainImporterError)

deriveToJSON defaultOptions ''BlockchainImporterError

instance FromJSON CEncodedSTx where
  parseJSON = withObject "signedTx" $ \v -> CEncodedSTx <$> v .: "signedTx"

-- JSON instance for ByteString, it is decoded from base 64
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
