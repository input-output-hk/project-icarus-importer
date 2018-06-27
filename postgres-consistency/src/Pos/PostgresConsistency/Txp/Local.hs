{-# LANGUAGE TypeFamilies #-}

-- | PostgresConsistency's local Txp.

module Pos.PostgresConsistency.Txp.Local
       ( eTxProcessTransaction
       , eTxProcessTransactionNoLock
       , eTxNormalize
       ) where

import           JsonLog (CanJsonLog (..))
import           Universum

import qualified Data.HashMap.Strict as HM

import           Pos.Core (BlockVersionData, EpochIndex, HasConfiguration, Timestamp)
import           Pos.Core.Txp (TxAux (..), TxId, TxUndo)
import           Pos.Slotting (MonadSlots (getCurrentSlot), getSlotStart)
import           Pos.StateLock (Priority (..), StateLock, StateLockMetrics, withStateLock)
import           Pos.Txp.Logic.Local (txNormalizeAbstract, txProcessTransactionAbstract)
import           Pos.Txp.MemState (MempoolExt, TxpLocalWorkMode, getTxpExtra, withTxpLocalData)
import           Pos.Txp.Toil (ToilVerFailure (..), Utxo)
import           Pos.Util.JsonLog.Events (MemPoolModifyReason (..))
import qualified Pos.Util.Modifier as MM
import           Pos.Util.Util (HasLens')

import           Pos.PostgresConsistency.Configuration (HasPostGresDB)
import           Pos.PostgresConsistency.Core (TxExtra (..))
import           Pos.PostgresConsistency.Txp.Toil (BlockchainImporterExtraModifier, ELocalToilM,
                                                   eDeletePendingTxs, eInsertPendingTx,
                                                   eNormalizeToil, eProcessTx, eemLocalTxsExtra)

type ETxpLocalWorkMode ctx m =
    ( TxpLocalWorkMode ctx m
    , MempoolExt m ~ BlockchainImporterExtraModifier
    )

eTxProcessTransaction ::
       ( ETxpLocalWorkMode ctx m
       , HasLens' ctx StateLock
       , HasLens' ctx (StateLockMetrics MemPoolModifyReason)
       , CanJsonLog m
       , HasConfiguration
       , HasPostGresDB
       )
    => (TxId, TxAux)
    -> m (Either ToilVerFailure ())
eTxProcessTransaction itw =
    withStateLock LowPriority ProcessTransaction $ \__tip -> eTxProcessTransactionNoLock itw

eTxProcessTransactionNoLock ::
       forall ctx m. (ETxpLocalWorkMode ctx m, HasConfiguration, HasPostGresDB)
    => (TxId, TxAux)
    -> m (Either ToilVerFailure ())
eTxProcessTransactionNoLock itw@(_, txAux) = getCurrentSlot >>= \case
    Nothing   -> pure $ Left ToilSlotUnknown
    Just slot -> do
        -- First get the current @SlotId@ so we can calculate the time.
        -- Then get when that @SlotId@ started and use that as a time for @Tx@.
        mTxTimestamp <- getSlotStart slot
        processRes <- txProcessTransactionAbstract buildEmptyContext (processTx' mTxTimestamp) itw
        forM processRes $ eInsertPendingTx (taTx txAux)
  where
    buildEmptyContext :: Utxo -> TxAux -> m ()
    buildEmptyContext _ _ = pure ()

    processTx' ::
           Maybe Timestamp
        -> BlockVersionData
        -> EpochIndex
        -> (TxId, TxAux)
        -> ExceptT ToilVerFailure ELocalToilM TxUndo
    processTx' mTxTimestamp bvd epoch tx =
        eProcessTx bvd epoch tx (TxExtra Nothing mTxTimestamp)

-- | 1. Recompute UtxoView by current MemPool
--   2. Remove invalid transactions from MemPool
--   3. Set new tip to txp local data
eTxNormalize ::
       forall ctx m. (ETxpLocalWorkMode ctx m, HasConfiguration, HasPostGresDB)
    => m ()
eTxNormalize = do
    extras <- MM.insertionsMap . view eemLocalTxsExtra <$> withTxpLocalData getTxpExtra
    invalidTxs <- txNormalizeAbstract buildEmptyContext (normalizeToil' extras)
    whenJust invalidTxs eDeletePendingTxs
  where
    buildEmptyContext :: Utxo -> [TxAux] -> m ()
    buildEmptyContext _ _ = pure ()

    normalizeToil' :: (MonadIO m, HasPostGresDB) =>
           HashMap TxId TxExtra
        -> BlockVersionData
        -> EpochIndex
        -> HashMap TxId TxAux
        -> ELocalToilM ([TxAux])
    normalizeToil' extras bvd epoch txs =
        let toNormalize = HM.toList $ HM.intersectionWith (,) txs extras
        in eNormalizeToil bvd epoch toNormalize
