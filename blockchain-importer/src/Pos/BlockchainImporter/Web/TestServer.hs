{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Pos.BlockchainImporter.Web.TestServer
       ( runMockServer
       ) where

import           Universum

import           Network.Wai (Application)
import           Network.Wai.Handler.Warp (run)
import           Servant.Generic (AsServerT, toServant)
import           Servant.Server (Handler, Server, serve)

import           Pos.BlockchainImporter.Aeson.ClientTypes ()
import           Pos.BlockchainImporter.Web.Api (BlockchainImporterApi,
                                                 BlockchainImporterApiRecord (..),
                                                 blockchainImporterApi)
import           Pos.BlockchainImporter.Web.ClientTypes (CEncodedSTx)
import           Pos.Web ()

----------------------------------------------------------------
-- Top level functionality
----------------------------------------------------------------

-- Run the server. Must be on the same port so we don't have to modify anything
runMockServer :: IO ()
runMockServer = run 8200 blockchainImporterApp

blockchainImporterApp :: Application
blockchainImporterApp = serve blockchainImporterApi blockchainImporterHandlers

----------------------------------------------------------------
-- Handlers
----------------------------------------------------------------

blockchainImporterHandlers :: Server BlockchainImporterApi
blockchainImporterHandlers =
    toServant (BlockchainImporterApiRecord
        { _blockCount         = getBlocksTotal
        , _sendSignedTx       = sendSignedTx
        }
        :: BlockchainImporterApiRecord (AsServerT Handler))

----------------------------------------------------------------
-- Test handlers
----------------------------------------------------------------

getBlocksTotal :: Handler Integer
getBlocksTotal = pure 10

sendSignedTx :: CEncodedSTx -> Handler ()
sendSignedTx = const $ pure ()
