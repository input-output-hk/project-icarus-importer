{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE InstanceSigs  #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

-- | Type-level specification of BlockchainImporter API (via Servant).

module Pos.BlockchainImporter.Web.Api
       ( BlockchainImporterApi
       , blockchainImporterApi
       , BlockchainImporterApiRecord(..)
       ) where

import           Universum

import           Control.Exception.Safe (try)
import           Data.Proxy (Proxy (Proxy))
import           Servant.API ((:>), Get, JSON, Post, ReqBody, Summary)
import           Servant.Generic ((:-), AsApi, ToServant)
import           Servant.Server (ServantErr (..))

import           Pos.BlockchainImporter.Web.ClientTypes (CEncodedSTx)
import           Pos.BlockchainImporter.Web.Error (BlockchainImporterError)
import           Pos.Util.Servant (ModifiesApiRes (..), VerbMod)

-- | API result modification mode used here.
data BlockchainImporterVerbTag

-- | Wrapper for Servants 'Verb' data type,
-- which allows to catch exceptions thrown by BlockchainImporter's endpoints.
type BlockchainImporterVerb verb = VerbMod BlockchainImporterVerbTag verb

-- | Shortcut for common api result types.
type ExRes verbMethod a = BlockchainImporterVerb (verbMethod '[JSON] a)

instance ModifiesApiRes BlockchainImporterVerbTag where
    type ApiModifiedRes BlockchainImporterVerbTag a = Either BlockchainImporterError a
    modifyApiResult
        :: Proxy BlockchainImporterVerbTag
        -> IO (Either ServantErr a)
        -> IO (Either ServantErr (Either BlockchainImporterError a))
    modifyApiResult _ action = try . try $ either throwM pure =<< action

-- | Servant API which provides access to blockchainImporter
type BlockchainImporterApi = "api" :> ToServant (BlockchainImporterApiRecord AsApi)

-- | Helper Proxy
blockchainImporterApi :: Proxy BlockchainImporterApi
blockchainImporterApi = Proxy

-- | A servant-generic record with all the methods of the API
data BlockchainImporterApiRecord route = BlockchainImporterApiRecord
  {
    _blockCount :: route
        :- Summary "Gets the total number of blocks imported (according to rocks db)."
        :> "stats"
        :> "blocksCount"
        :> ExRes Get Integer

  , _sendSignedTx :: route
        :- Summary "Broadcasts a signed tx to the network."
        :> "txs"
        :> "signed"
        :> ReqBody '[JSON] CEncodedSTx
        :> ExRes Post ()
  }
  deriving (Generic)
