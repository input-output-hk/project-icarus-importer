{-# LANGUAGE TypeFamilies #-}

-- | BlockchainImporter's version of Toil logic.

module Pos.BlockchainImporter.Txp.Toil.Logic
      ( -- * Block processing
        eApplyToil
      , eRollbackToil
        -- * Tx processing
      , eNormalizeToil
      , eProcessTx
        -- * Failed tx processing
      , eFailedToil
      , eCheckSuccessfulToil
      ) where

import           Universum

import           Control.Monad.Except (mapExceptT)
import qualified Database.PostgreSQL.Simple as PGS

import           Pos.BlockchainImporter.Configuration (HasPostGresDB, postGreOperate)
import           Pos.BlockchainImporter.Core (TxExtra (..))
import qualified Pos.BlockchainImporter.Tables.BestBlockTable as BBT
import qualified Pos.BlockchainImporter.Tables.TxsTable as TxsT
import qualified Pos.BlockchainImporter.Tables.UtxosTable as UT
import           Pos.BlockchainImporter.Txp.Toil.Monad (EGlobalToilM, ELocalToilM)
import           Pos.Core (BlockCount, BlockVersionData, EpochIndex, HasConfiguration, Timestamp)
import           Pos.Core.Txp (Tx (..), TxAux (..), TxId, TxIn (..), TxOutAux (..), TxUndo)
import           Pos.Crypto (WithHash (..), hash)
import           Pos.DB.Class (MonadDBRead)
import           Pos.Txp.Configuration (HasTxpConfiguration)
import           Pos.Txp.DB.Utxo (getTxOut)
import           Pos.Txp.Settings (IsNewEpochOperation (..))
import           Pos.Txp.Toil (ToilVerFailure (..), extendGlobalToilM, extendLocalToilM)
import qualified Pos.Txp.Toil as Txp
import           Pos.Txp.Topsort (topsortTxs)
import qualified Pos.Util.Modifier as MM

----------------------------------------------------------------------------
-- Block processing
----------------------------------------------------------------------------

-- | Apply transactions from one block or genesis. They must be valid (for
-- example, it implies topological sort).
eApplyToil ::
       forall m. (HasConfiguration, HasPostGresDB, MonadIO m, MonadDBRead m)
    => IsNewEpochOperation    -- Whether apply resulted from new epoch operation
    -> Maybe Timestamp        -- Timestamp of the block
    -> [(TxAux, TxUndo)]      -- Txs of the block
    -> Maybe BlockCount       -- Number of the block, if it's not a genesis
    -> m (EGlobalToilM ())
eApplyToil isNewEpoch mTxTimestamp txun maybeBlockHeight = do
    -- Genesis block changes don't impact postgresdb
    whenJust maybeBlockHeight $ eApplyToilPG isNewEpoch mTxTimestamp txun
    pure $ extendGlobalToilM $ Txp.applyToil txun

eApplyToilPG ::
     forall m. (HasConfiguration, HasPostGresDB, MonadIO m, MonadDBRead m)
  => IsNewEpochOperation
  -> Maybe Timestamp
  -> [(TxAux, TxUndo)]
  -> BlockCount
  -> m ()
eApplyToilPG isNewEpoch mTxTimestamp txun blockHeight = do
    -- Update best block
    postgresStoreOnBlockEvent isNewEpoch $
                              BBT.updateBestBlock blockHeight

    -- Update UTxOs
    postgresStoreOnBlockEvent isNewEpoch $
                              UT.applyModifierToUtxos $ applyUTxOModifier txun

    -- Update tx history
    mapM_ applier txun
  where
    applier :: (TxAux, TxUndo) -> m ()
    applier (txAux, txUndo) = do
        let tx = taTx txAux
            newExtra = TxExtra mTxTimestamp txUndo

        postgresStoreOnBlockEvent isNewEpoch $
                                  TxsT.upsertSuccessfulTx tx newExtra blockHeight


-- | Rollback transactions from one block or genesis.
eRollbackToil ::
       forall m. (HasConfiguration, HasPostGresDB, MonadIO m, MonadDBRead m)
    => IsNewEpochOperation    -- Whether rollback resulted from new epoch operation
    -> [(TxAux, TxUndo)]      -- Txs of the block
    -> Maybe BlockCount       -- Number of the block, if it's not a genesis
    -> m (EGlobalToilM ())
eRollbackToil isNewEpoch txun maybeBlockHeight = do
    -- Genesis block changes don't impact postgresdb
    whenJust maybeBlockHeight $ eRollbackToilPG isNewEpoch txun
    pure $ extendGlobalToilM $ Txp.rollbackToil txun

eRollbackToilPG ::
     forall m. (HasConfiguration, HasPostGresDB, MonadIO m, MonadDBRead m)
  => IsNewEpochOperation
  -> [(TxAux, TxUndo)]
  -> BlockCount
  -> m ()
eRollbackToilPG isNewEpoch txun blockHeight = do
    -- Update best block
    postgresStoreOnBlockEvent isNewEpoch $
                              BBT.updateBestBlock (blockHeight - 1)

    -- Update UTxOs
    postgresStoreOnBlockEvent isNewEpoch $
                              UT.applyModifierToUtxos $ rollbackUTxOModifier txun

    -- Update tx history
    mapM_ extraRollback $ reverse txun
  where
    extraRollback :: (TxAux, TxUndo) -> m ()
    extraRollback (txAux, txUndo) = do
        let tx      = taTx txAux

        postgresStoreOnBlockEvent isNewEpoch $
                                  TxsT.upsertPendingTx tx txUndo


----------------------------------------------------------------------------
-- Tx processing
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


----------------------------------------------------------------------------
-- Failed tx processing
----------------------------------------------------------------------------

{-| Upserts a failed tx into the tx history table

    Note that the tx that failed could have not being pending, as is the case where it failed
    during it's processing. In that case, we attempt to obtain the inputs, returning them only
    if we successfully get all of them
-}
eFailedToil :: (MonadIO m, MonadDBRead m, HasPostGresDB) => Tx -> m ()
eFailedToil tx = do
  inputs <- fetchTxSenders tx
  liftIO $ postGreOperate $ TxsT.upsertFailedTx tx inputs

-- | Checks if a tx was successful
eCheckSuccessfulToil :: (MonadIO m, MonadDBRead m, HasPostGresDB) => Tx -> m Bool
eCheckSuccessfulToil tx = do
  maybeTx <- liftIO $ postGreOperate $ TxsT.getTxByHash (hash tx)
  pure $ case maybeTx of
        Just (TxsT.TxRecord _ _ _ _ _ TxsT.Successful) -> True
        _                                              -> False


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

fetchTxSenders :: (MonadIO m, MonadDBRead m, HasPostGresDB) => Tx -> m TxUndo
fetchTxSenders tx = do
  let txId = hash tx
  maybeTx <- liftIO $ postGreOperate $ TxsT.getTxByHash txId
  case maybeTx of
    Just pgTx ->
      pure $ Just <$> TxsT.txInputs pgTx
    Nothing -> do
      -- Fetch the tx inputs
      -- If any of the inputs of the tx is not known, none is returned
      knownInputs <- mapM getTxOut (_txInputs tx)
      let allInputsKnown = all isJust knownInputs
          inputs         = if allInputsKnown then knownInputs
                           else const Nothing <$> _txInputs tx
      pure inputs

postgresStoreOnBlockEvent ::
     (MonadIO m, HasPostGresDB)
  => IsNewEpochOperation
  -> (PGS.Connection -> IO ())
  -> m ()
postgresStoreOnBlockEvent isNewEpoch op = case isNewEpoch of
  (IsNewEpochOperation True)  -> pure ()
  (IsNewEpochOperation False) -> liftIO $ postGreOperate op
