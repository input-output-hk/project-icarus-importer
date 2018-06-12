{-# LANGUAGE TypeFamilies #-}

-- | BlockchainImporter's version of Toil logic.

module Pos.BlockchainImporter.Txp.Toil.Logic
      ( -- * Block processing
        eApplyToil
      , eRollbackToil
        -- * Tx processing
      , eNormalizeToil
      , eProcessTx
        -- * Pending tx DB processing
      , eInsertPendingTx
      , eDeletePendingTxs
      ) where

import           Universum

import           Control.Monad.Except (mapExceptT)

import           Pos.BlockchainImporter.Configuration (HasPostGresDB, maybePostGreStore,
                                                       postGreStore)
import           Pos.BlockchainImporter.Core (TxExtra (..))
import qualified Pos.BlockchainImporter.Tables.BestBlockTable as BBT
import qualified Pos.BlockchainImporter.Tables.PendingTxsTable as PTxsT
import qualified Pos.BlockchainImporter.Tables.TxsTable as TxsT
import qualified Pos.BlockchainImporter.Tables.UtxosTable as UT
import           Pos.BlockchainImporter.Txp.Toil.Monad (EGlobalToilM, ELocalToilM)
import           Pos.Core (BlockVersionData, EpochIndex, HasConfiguration, HeaderHash, Timestamp)
import           Pos.Core.Txp (Tx (..), TxAux (..), TxId, TxIn (..), TxOutAux (..), TxUndo)
import           Pos.Crypto (WithHash (..), hash)
import           Pos.Txp.Configuration (HasTxpConfiguration)
import           Pos.Txp.Toil (ToilVerFailure (..), extendGlobalToilM, extendLocalToilM)
import qualified Pos.Txp.Toil as Txp
import           Pos.Txp.Topsort (topsortTxs)
import qualified Pos.Util.Modifier as MM

----------------------------------------------------------------------------
-- Global
----------------------------------------------------------------------------

-- | Apply transactions from one block. They must be valid (for
-- example, it implies topological sort).
eApplyToil ::
       forall m. (HasConfiguration, HasPostGresDB, MonadIO m)
    => Maybe Timestamp
    -> [(TxAux, TxUndo)]
    -> (HeaderHash, Word64)
    -> m (EGlobalToilM ())
eApplyToil mTxTimestamp txun (hh, blockHeight) = do
    -- Update best block
    liftIO $ maybePostGreStore blockHeight $ BBT.updateBestBlock blockHeight

    -- Update UTxOs
    let toilApplyUTxO = extendGlobalToilM $ Txp.applyToil txun

    liftIO $ maybePostGreStore blockHeight $ UT.applyModifierToUtxos $ applyUTxOModifier txun

    -- Update tx history
    zipWithM_ (curry applier) [0..] txun
    return toilApplyUTxO
  where
    applier :: (Word32, (TxAux, TxUndo)) -> m ()
    applier (i, (txAux, txUndo)) = do
        let tx = taTx txAux
            newExtra = TxExtra (Just (hh, i)) mTxTimestamp txUndo

        liftIO $ maybePostGreStore blockHeight $ TxsT.insertTx tx newExtra blockHeight
        eDeletePendingTxs [txAux]

-- | Rollback transactions from one block.
eRollbackToil ::
     forall m. (HasConfiguration, HasPostGresDB, MonadIO m)
  => [(TxAux, TxUndo)] -> Word64 -> m (EGlobalToilM ())
eRollbackToil txun blockHeight = do
    -- Update best block
    liftIO $ maybePostGreStore blockHeight $ BBT.updateBestBlock (blockHeight - 1)

    -- Update UTxOs
    let toilRollbackUtxo = extendGlobalToilM $ Txp.rollbackToil txun

    liftIO $ maybePostGreStore blockHeight $ UT.applyModifierToUtxos $ rollbackUTxOModifier txun

    -- Update tx history
    mapM_ extraRollback $ reverse txun
    return toilRollbackUtxo
  where
    extraRollback :: (TxAux, TxUndo) -> m ()
    extraRollback (txAux, txUndo) = do
        let tx      = taTx txAux

        liftIO $ maybePostGreStore blockHeight $ TxsT.deleteTx tx
        eInsertPendingTx tx txUndo

----------------------------------------------------------------------------
-- Local
----------------------------------------------------------------------------

-- | Verify one transaction and also add it to mem pool and apply to utxo
-- if transaction is valid.
eProcessTx ::
       (HasTxpConfiguration, HasConfiguration)
    => BlockVersionData
    -> EpochIndex
    -> (TxId, TxAux)
    -> (TxUndo -> TxExtra)
    -> ExceptT ToilVerFailure ELocalToilM TxUndo
eProcessTx bvd curEpoch tx _ = mapExceptT extendLocalToilM $ Txp.processTx bvd curEpoch tx

-- | Get rid of invalid transactions.
-- All valid transactions will be added to mem pool and applied to utxo.
eNormalizeToil ::
       (HasTxpConfiguration, HasConfiguration, HasPostGresDB)
    => BlockVersionData
    -> EpochIndex
    -> [(TxId, (TxAux, TxExtra))]
    -> ELocalToilM ([TxAux])
eNormalizeToil bvd curEpoch txs = catMaybes <$> mapM normalize ordered
  where
    ordered = fromMaybe txs $ topsortTxs wHash txs
    wHash (i, (txAux, _)) = WithHash (taTx txAux) i
    normalize (i, (txAux, extra)) = do
      res <- runExceptT $ uncurry (eProcessTx bvd curEpoch) $ repair (i, (txAux, extra))
      pure $ txAux <$ leftToMaybe res
    repair (i, (txAux, extra)) = ((i, txAux), const extra)

-- | Inserts a pending tx to the Postgres DB
eInsertPendingTx ::
       (MonadIO m, HasPostGresDB)
    => Tx
    -> TxUndo
    -> m ()
eInsertPendingTx tx txUndo = liftIO $ postGreStore $ PTxsT.insertPendingTx tx txUndo

-- | Deletes a pending tx from the Postgres DB
eDeletePendingTxs :: (MonadIO m, HasPostGresDB) => [TxAux] -> m ()
eDeletePendingTxs txAuxs = mapM_ (liftIO . postGreStore . PTxsT.deletePendingTx . taTx) txAuxs

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- Returns the UxtoModifier corresponding to applying a list of txs
applyUTxOModifier :: [(TxAux, TxUndo)] -> Txp.UtxoModifier
applyUTxOModifier txs = mconcat $ applySingleModifier <$> txs

-- Returns the UxtoModifier corresponding to applying a single tx
applySingleModifier :: (TxAux, TxUndo) -> Txp.UtxoModifier
applySingleModifier (txAux, _) = foldr  MM.delete
                                        (foldr (uncurry MM.insert) mempty toInsert)
                                        toDelete
  where tx       = taTx txAux
        id       = hash tx
        outputs  = toList $ _txOutputs tx
        toInsert = zipWith (\o index -> (TxInUtxo id index, TxOutAux o)) outputs [0..]
        toDelete = toList $ _txInputs tx

-- Returns the UxtoModifier corresponding to rollbacking a list of txs
rollbackUTxOModifier :: [(TxAux, TxUndo)] -> Txp.UtxoModifier
rollbackUTxOModifier txs = mconcat $ rollbackSingleModifier <$> reverse txs

-- Returns the UxtoModifier corresponding to rollbacking a single tx
rollbackSingleModifier :: (TxAux, TxUndo) -> Txp.UtxoModifier
rollbackSingleModifier (txAux, txUndo) = foldr  MM.delete
                                                (foldr (uncurry MM.insert) mempty toInsert)
                                                toDelete
  where tx       = taTx txAux
        id       = hash tx
        inputs   = toList $ _txInputs tx
        outputs  = toList $ _txOutputs tx
        toDelete = [ TxInUtxo id (fromIntegral index) | index <- [0..length outputs - 1] ]
        toInsert = catMaybes $ zipWith mapValueToMaybe inputs $ toList txUndo

        mapValueToMaybe :: a -> Maybe b -> Maybe (a, b)
        mapValueToMaybe a = fmap ((,) a)
