{-# LANGUAGE OverloadedLists #-}

-- | Instances of `ToSchema` & `ToParamSchema`

module Pos.Wallet.Web.Swagger.Instances.Schema where

import           Universum

import           Control.Lens (ix, mapped, (?~))
import           Crypto.Hash (Digest)
import           Data.Aeson (toJSON)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Swagger (NamedSchema (..), SwaggerType (..), ToParamSchema (..),
                               ToSchema (..), declareNamedSchema, declareSchema, declareSchemaRef,
                               defaultSchemaOptions, description, example, format,
                               genericDeclareNamedSchema, minItems, name, properties, required,
                               sketchSchema, type_)
import           Data.Swagger.Internal.Schema (GToSchema, binarySchema, gdeclareNamedSchema, named)
import qualified Data.Swagger.Lens as Swagger
import           Data.Typeable (Typeable, typeRep)
import           Data.Version (Version)
import qualified GHC.Generics as G
import           Servant.Multipart (FileData (..))

import           Cardano.Crypto.Wallet (ChainCode (..), XPub, XSignature)
import qualified Crypto.Sign.Ed25519 as ED (PublicKey, Signature)
import           Pos.Client.Txp.Util (InputSelectionPolicy (..))
import           Pos.Core (AddrAttributes (..), AddrStakeDistribution (..), AddrType (..),
                           Address (..), ApplicationName, BlockCount (..), BlockVersion,
                           ChainDifficulty, Coin, CoinPortion (..), Script (..), SlotCount (..),
                           SoftwareVersion, mkCoin)
import           Pos.Core.Txp (TxIn (..), TxInWitness (..), TxOut (..), TxOutAux (..),
                               TxSigData (..))
import           Pos.Crypto.Hashing (AbstractHash (..))
import           Pos.Crypto.HD (HDAddressPayload (..))
import           Pos.Crypto.Signing (PublicKey (..), RedeemPublicKey, RedeemSignature (..),
                                     Signature (..))
import           Pos.Data.Attributes (Attributes (..), UnparsedFields (..))
import           Pos.Util.BackupPhrase (BackupPhrase)

import qualified Pos.Wallet.Web.ClientTypes as CT
import qualified Pos.Wallet.Web.Error.Types as ET

import           Pos.Wallet.Web.Methods.Misc (PendingTxsSummary, WalletStateSnapshot)

-- | Instances we need to build Swagger-specification for 'walletApi':
-- 'ToParamSchema' - for types in parameters ('Capture', etc.),
-- 'ToSchema' - for types in bodies.

-- | This orphan instance prevents Generic-based deriving mechanism
-- to use 'ToSchema' 'ByteString' and instead defaults to 'binarySchema'.
instance GToSchema (G.K1 i BS.ByteString) where
  gdeclareNamedSchema _ _ _ = pure $ NamedSchema Nothing binarySchema

instance GToSchema (G.K1 i LBS.ByteString) where
  gdeclareNamedSchema _ _ _ = pure $ NamedSchema Nothing binarySchema

instance ToSchema (Digest algo) where
  declareNamedSchema _ = pure $ NamedSchema Nothing binarySchema

instance ToSchema UnparsedFields where
  declareNamedSchema _ = pure $ NamedSchema Nothing binarySchema

instance ToSchema XSignature where
  declareNamedSchema _ = pure $ NamedSchema Nothing binarySchema

instance ToSchema a => ToSchema (Attributes a)
instance ToSchema      (AbstractHash algo a)
instance ToSchema      TxIn
instance ToSchema      TxOut
instance ToSchema      TxOutAux
instance ToSchema      Address
instance ToSchema      HDAddressPayload
instance ToSchema      AddrStakeDistribution
instance ToSchema      CoinPortion
instance ToSchema      AddrAttributes
instance ToSchema      AddrType
instance ToSchema      PublicKey
instance ToSchema      ED.Signature
instance ToSchema      ED.PublicKey
instance ToSchema      XPub
deriving instance Generic ChainCode
instance ToSchema      ChainCode
instance ToSchema      Script
instance ToSchema      RedeemPublicKey
instance ToSchema      (Signature TxSigData)
instance ToSchema      (RedeemSignature TxSigData)
instance ToSchema      TxInWitness
instance ToSchema      Coin
instance ToParamSchema Coin
instance ToSchema      CT.CTxId
instance ToParamSchema CT.CTxId
instance ToSchema      CT.CTx
instance ToSchema      CT.CTxMeta
instance ToSchema      CT.CPtxCondition
instance ToSchema      CT.CHash
instance ToParamSchema CT.CHash
instance ToSchema      (CT.CId CT.Wal)
instance ToSchema      (CT.CId CT.Addr)
instance ToParamSchema (CT.CId CT.Wal)
instance ToParamSchema (CT.CId CT.Addr)
instance ToSchema      CT.CProfile
instance ToSchema      ET.WalletError

instance ToSchema      CT.CAccountId
instance ToParamSchema CT.CAccountId where
    toParamSchema _ = mempty
        & type_ .~ SwaggerString
        & format ?~ "walletSetAddress@walletKeyIndex"

instance ToSchema      CT.CWalletAssurance
instance ToSchema      CT.CAccountMeta
instance ToSchema      CT.CWalletMeta
instance ToSchema      CT.CAccountInit
instance ToSchema      CT.CWalletInit
instance ToSchema      CT.CWalletRedeem
instance ToSchema      CT.CWallet
instance ToSchema      CT.CAccount
instance ToSchema      CT.CAddress
instance ToSchema      CT.CPaperVendWalletRedeem
instance ToSchema      CT.CCoin
instance ToSchema      CT.CInitialized
instance ToSchema      CT.CElectronCrashReport
instance ToSchema      CT.CUpdateInfo
instance ToSchema      SoftwareVersion
instance ToSchema      ApplicationName
instance ToSchema      CT.SyncProgress
instance ToSchema      BlockCount
instance ToSchema      SlotCount
instance ToSchema      ChainDifficulty
instance ToSchema      BlockVersion
instance ToSchema      BackupPhrase
instance ToParamSchema CT.CPassPhrase
instance ToParamSchema CT.ScrollOffset
instance ToParamSchema CT.ScrollLimit
instance ToSchema      CT.ApiVersion
instance ToSchema      Version
instance ToSchema      CT.ClientInfo
instance ToSchema      CT.CEncodedData
instance ToSchema      CT.CSignedEncTx
instance ToSchema      CT.CUtxo

instance ToSchema InputSelectionPolicy where
    declareNamedSchema proxy =
        genericDeclareNamedSchema defaultSchemaOptions proxy
            & mapped.Swagger.schema.example ?~ toJSON OptimizeForHighThroughput

instance ToSchema WalletStateSnapshot where
    declareNamedSchema _ = pure $ NamedSchema (Just "WalletStateSnapshot") mempty

instance ToSchema PendingTxsSummary where
    declareNamedSchema _ = pure $ NamedSchema (Just "PendingTxsSummary") mempty

instance ToSchema (FileData tag) where
    declareNamedSchema _ = do
        textSchema <- declareSchemaRef (Proxy :: Proxy Text)
        filepathSchema <- declareSchemaRef (Proxy :: Proxy FilePath)
        return $ NamedSchema (Just "FileData") $ mempty
            & type_ .~ SwaggerObject
            & properties .~
                [ ("fdInputFile", textSchema)
                , ("fdFileName", textSchema)
                , ("fdFileCType", textSchema)
                , ("fdFilePath", filepathSchema)
                ]
            & required .~ [ "fdInputFile", "fdFileName", "fdFileCType", "fdFilePath"]

instance ToSchema CT.CFilePath where
    declareNamedSchema _ = do
        schema <- declareSchema (Proxy :: Proxy FilePath)
        let schema' = schema
                & description ?~ desc
                & example ?~ "keys/1.key"
        return $ named "FilePath" schema'
      where
        desc = "Path to file.\n \
               \Note that it is represented as JSON-string, \
               \one may need to properly escape content. For instance:\n \
               \curl ... -X \"\\\\\"1.key\"\\\\\".\n\
               \Also, when on Windows, don't forget to double-escape \
               \ backslashes, e.g. \
               \ \"C:\\\\\\\\\\\\\\\\keys\\\\\\\\1.key\""

instance ToSchema CT.NewBatchPayment where
    declareNamedSchema _ = do
        cAccountIdSchema <- declareSchemaRef (Proxy @CT.CAccountId)
        return $ NamedSchema (Just "NewBatchPayment") $
            sketchSchema example_
                & properties . ix "npbFrom" .~ cAccountIdSchema
      where
        example_ = CT.NewBatchPayment
            { CT.npbFrom = CT.CAccountId "<walletId@accountId>"
            , CT.npbTo   = (CT.CId (CT.CHash "<address>"), mkCoin 228) :|
                          [(CT.CId (CT.CHash "<address>"), mkCoin 701)]
            , CT.npbInputSelectionPolicy = OptimizeForSecurity
            }

-- | Instance for Either-based types (types we return as 'Right') in responses.
-- Due 'typeOf' these types must be 'Typeable'.
-- We need this instance for correct Swagger-specification.
instance {-# OVERLAPPING #-}
         (Typeable a, ToSchema a) =>
         ToSchema (Either ET.WalletError a) where
    declareNamedSchema proxy =
        genericDeclareNamedSchema defaultSchemaOptions proxy
            & mapped . name ?~ show (typeRep $ Proxy @(Either ET.WalletError a))

instance ToSchema a => ToSchema (NonEmpty a) where
    declareNamedSchema _ = do
        schema <- declareSchema (Proxy :: Proxy [a])
        pure $ NamedSchema Nothing $ schema
            & minItems ?~ 1
