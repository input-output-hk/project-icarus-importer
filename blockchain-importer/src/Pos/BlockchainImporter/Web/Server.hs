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

import           Control.Error.Util (exceptT, failWith, hoistEither)
import           Control.Monad.Except (ExceptT, withExceptT)
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
import           Pos.Core.Txp (TxAux, taTx)
import           Pos.Txp (MonadTxpLocal, ToilVerFailure (..), TxId, txpProcessTx, verifyTx)
import           Pos.Txp.DB.Utxo (getTxOut)
import           Pos.Web (serveImpl)

import           Pos.BlockchainImporter.Aeson.ClientTypes ()
import           Pos.BlockchainImporter.BlockchainImporterMode (BlockchainImporterMode)
import           Pos.BlockchainImporter.Configuration (withPostGreTransactionM)
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
        , _sendSignedTx       = sendEncodedSignedTx _diffusion
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


sendEncodedSignedTx
     :: (BlockchainImporterMode ctx m, MonadTxpLocal m)
     => Diffusion m
     -> CEncodedSTx
     -> m ()
sendEncodedSignedTx diff@Diffusion{..} encodedSTx =
  exceptT' (hoistEither $ decodeSTx encodedSTx) (const $ throwM eInvalidEnc) $ \txAux ->
    -- FIXME: We are using only the confirmed UTxO, we should also take into account the pending txs
    exceptT' (sendSignedTx diff txAux) (handleSendSTxError txAux) (const $ pure ())
    where eInvalidEnc = Internal "Tx not broadcasted: invalid encoded tx"
          exceptT' e f g = exceptT f g e


data SendSTxFailure = InvalidTx ToilVerFailure
                    | TxProcessFailed ToilVerFailure
                    | TxNotAccepted
                    deriving Eq

sendSignedTx
     :: (BlockchainImporterMode ctx m, MonadTxpLocal m)
     => Diffusion m
     -> TxAux
     -> ExceptT SendSTxFailure m ()
sendSignedTx Diffusion {..} txAux = do
  let txHash = hash $ taTx txAux
  _ <- withExceptT InvalidTx $ verifyTx getTxOut False txAux
  _ <- withExceptT TxProcessFailed $ ExceptT $ txpProcessTx (txHash, txAux)
  -- FIXME: Replace 6 second limit with a lower value?
  -- This is done for two reasons:
  -- 1. In order not to overflow relay.
  -- 2. To let other things (e. g. block processing) happen if
  -- `newPayment`s are done continuously.
  wasAccepted <- lift $ notFasterThan (6 :: Second) $ sendTx txAux
  void $ unless wasAccepted $ (failWith TxNotAccepted Nothing)

handleSendSTxError ::
     (BlockchainImporterMode ctx m, MonadTxpLocal m)
  => TxAux -> SendSTxFailure -> m a
handleSendSTxError txAux sendErr = do
  currTime <- getCurrentTimestamp
  -- Remove pending tx if the tx is not already pending
  unless  (sendErr == TxProcessFailed ToilKnown) $
          withPostGreTransactionM $ eApplyFailedTx (taTx txAux) (Just currTime)
  let txHash = hash $ taTx txAux
  throwM $ sendSTxFailureToBIError txHash sendErr


--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | A pure function that return the number of blocks.
getBlockDifficulty :: Block -> Integer
getBlockDifficulty tipBlock = fromIntegral $ getChainDifficulty $ tipBlock ^. difficultyL

sendSTxFailureToBIError :: TxId -> SendSTxFailure -> BlockchainImporterError
sendSTxFailureToBIError txHash failure = Internal $ case failure of
  InvalidTx reason -> sformat ("Tx not broadcasted "%build%": "%build)
                              txHash reason
  TxProcessFailed err -> sformat  ("Tx not broadcasted "%build%": error during process "%build)
                                  txHash err
  TxNotAccepted -> sformat  ("Tx broadcasted "%build%", not accepted by any peer") txHash

----------------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------------

notFasterThan ::
       (Mockable Concurrently m, Mockable Delay m) => Second -> m a -> m a
notFasterThan time action = fst <$> concurrently action (delay time)
