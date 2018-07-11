{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}

-- API server logic

module Pos.BlockchainImporter.Web.Server
       ( blockchainImporterServeImpl
       , blockchainImporterApp
       , blockchainImporterHandlers

       -- pure functions
       , getBlockDifficulty

       -- api functions
       , getBlocksTotal
       ) where

import           Universum

import           Control.Error.Util (exceptT, hoistEither)
import           Data.Time.Units (Second)
import           Formatting (build, sformat, (%))
import           Mockable (Concurrently, Delay, Mockable, concurrently, delay)
import           Network.Wai (Application)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)

import           Servant.Generic (AsServerT, toServant)
import           Servant.Server (Server, ServerT, serve)

import           Pos.Crypto (hash)

import           Pos.Diffusion.Types (Diffusion (..))

import           Pos.Core (difficultyL, getChainDifficulty, getCurrentTimestamp)
import           Pos.Core.Block (Block)
import           Pos.Core.Txp (TxAux, TxId, taTx)
import           Pos.Txp (MonadTxpLocal, txpProcessTx, verifyTx)
import           Pos.Txp.DB.Utxo (getTxOut)
import           Pos.Web (serveImpl)

import           Pos.BlockchainImporter.Aeson.ClientTypes ()
import           Pos.BlockchainImporter.BlockchainImporterMode (BlockchainImporterMode)
import           Pos.BlockchainImporter.ExtraContext (HasBlockchainImporterCSLInterface (..))
import           Pos.BlockchainImporter.Txp.Toil (eApplyFailedTx)
import           Pos.BlockchainImporter.Web.Api (BlockchainImporterApi,
                                                 BlockchainImporterApiRecord (..),
                                                 blockchainImporterApi)
import           Pos.BlockchainImporter.Web.ClientTypes (CEncodedSTx (..), decodeSTx)
import           Pos.BlockchainImporter.Web.Error (BlockchainImporterError (..))


----------------------------------------------------------------
-- Top level functionality
----------------------------------------------------------------

blockchainImporterServeImpl
    :: BlockchainImporterMode ctx m
    => m Application
    -> Word16
    -> m ()
blockchainImporterServeImpl app port = serveImpl loggingApp "*" port Nothing Nothing Nothing
  where
    loggingApp = logStdoutDev <$> app

blockchainImporterApp :: BlockchainImporterMode ctx m => m (Server BlockchainImporterApi) -> m Application
blockchainImporterApp serv = serve blockchainImporterApi <$> serv

----------------------------------------------------------------
-- Handlers
----------------------------------------------------------------

blockchainImporterHandlers
    :: forall ctx m. (BlockchainImporterMode ctx m, MonadTxpLocal m)
    => Diffusion m -> ServerT BlockchainImporterApi m
blockchainImporterHandlers _diffusion =
    toServant (BlockchainImporterApiRecord
        { _blockCount         = getBlocksTotal
        , _sendSignedTx       = sendSignedTx(_diffusion)
        }
        :: BlockchainImporterApiRecord (AsServerT m))

----------------------------------------------------------------
-- API Functions
----------------------------------------------------------------

-- | Get the total number of blocks/slots currently available.
-- Total number of main blocks   = difficulty of the topmost (tip) header.
-- Total number of anchor blocks = current epoch + 1
getBlocksTotal
    :: BlockchainImporterMode ctx m
    => m Integer
getBlocksTotal = do
    -- Get the tip block.
    tipBlock <- getTipBlockCSLI
    pure $ getBlockDifficulty tipBlock


sendSignedTx
     :: (BlockchainImporterMode ctx m, MonadTxpLocal m)
     => Diffusion m
     -> CEncodedSTx
     -> m ()
sendSignedTx diff@Diffusion{..} encodedSTx =
  exceptT' (hoistEither $ decodeSTx encodedSTx) (const $ throwM eInvalidEnc) $ \txAux -> do
    let txHash = hash $ taTx txAux
    -- FIXME: We are using only the confirmed UTxO, we should also take into account the pending txs
    exceptT' (verifyTx getTxOut False txAux) (throwM . eInvalidTx txHash) $ \_ ->
        catch (sendVerifiedTx diff txHash txAux) (handleSendVerifiedTxError txAux)
        where eInvalidEnc = Internal "Tx not broadcasted: invalid encoded tx"
              eInvalidTx txHash reason = Internal $
                  sformat ("Tx not broadcasted "%build%": "%build) txHash reason
              exceptT' e f g = exceptT f g e

sendVerifiedTx
     :: (BlockchainImporterMode ctx m, MonadTxpLocal m)
     => Diffusion m
     -> TxId
     -> TxAux
     -> m ()
sendVerifiedTx Diffusion {..} txHash txAux = do
  txProcessRes <- txpProcessTx (txHash, txAux)
  whenLeft txProcessRes $ throwM . eProcessErr
  -- FIXME: Replace 6 second limit with a lower value?
  -- This is done for two reasons:
  -- 1. In order not to overflow relay.
  -- 2. To let other things (e. g. block processing) happen if
  -- `newPayment`s are done continuously.
  wasAccepted <- notFasterThan (6 :: Second) $ sendTx txAux
  void $ unless wasAccepted $ (throwM $ eNotAccepted)
    where eProcessErr err = Internal $
              sformat ("Tx not broadcasted "%build%": error during process "%build) txHash err
          eNotAccepted = Internal $
              sformat  ("Tx broadcasted "%build%", not accepted by any peer") txHash

handleSendVerifiedTxError ::
     (BlockchainImporterMode ctx m, MonadTxpLocal m)
  => TxAux -> BlockchainImporterError -> m a
handleSendVerifiedTxError txAux biError = do
  currTime <- getCurrentTimestamp
  eApplyFailedTx (taTx txAux) (Just currTime)
  throwM biError


--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | A pure function that return the number of blocks.
getBlockDifficulty :: Block -> Integer
getBlockDifficulty tipBlock = fromIntegral $ getChainDifficulty $ tipBlock ^. difficultyL


----------------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------------

notFasterThan ::
       (Mockable Concurrently m, Mockable Delay m) => Second -> m a -> m a
notFasterThan time action = fst <$> concurrently action (delay time)
