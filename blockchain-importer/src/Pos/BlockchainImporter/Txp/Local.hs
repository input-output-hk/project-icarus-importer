{-# LANGUAGE TypeFamilies #-}

-- | BlockchainImporter's local Txp.

module Pos.BlockchainImporter.Txp.Local
       ( eTxProcessTransaction
       , eTxProcessTransactionNoLock
       , eTxNormalize
       ) where

import           JsonLog (CanJsonLog (..))
import           Universum

import qualified Data.HashMap.Strict as HM
import           UnliftIO (MonadUnliftIO)

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

import           Pos.BlockchainImporter.Configuration (HasPostGresDB, postGreOperate,
                                                       withPostGreTransaction,
                                                       withPostGreTransactionM)
import           Pos.BlockchainImporter.Core (TxExtra (..))
import           Pos.BlockchainImporter.Tables.TxsTable as TxsT
import           Pos.BlockchainImporter.Txp.Toil (BlockchainImporterExtraModifier, ELocalToilM,
                                                  eApplyFailedTx, eNormalizeToil, eProcessTx,
                                                  eemLocalTxsExtra)

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
        forM  processRes $
              liftIO . withPostGreTransaction . postGreOperate . (TxsT.upsertPendingTx (taTx txAux))
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
        eProcessTx bvd epoch tx (TxExtra mTxTimestamp)

-- | 1. Recompute UtxoView by current MemPool
--   2. Remove invalid transactions from MemPool
--   3. Set new tip to txp local data
eTxNormalize ::
       forall ctx m. (ETxpLocalWorkMode ctx m, HasConfiguration, HasPostGresDB, MonadUnliftIO m)
    => m ()
eTxNormalize = withPostGreTransactionM $ do
    extras <- MM.insertionsMap . view eemLocalTxsExtra <$> withTxpLocalData getTxpExtra
    invalidTxs <- txNormalizeAbstract buildEmptyContext (normalizeToil' extras)
    whenJust invalidTxs $ mapM_ (eApplyFailedTx . taTx)
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
