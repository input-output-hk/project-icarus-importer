-- | BlockchainImporter's global Txp (expressed as settings).

module Pos.BlockchainImporter.Txp.Global
       ( blockchainImporterTxpGlobalSettings
       ) where

import           Universum

import           Pos.Core (BlockCount, ComponentBlock (..), HasConfiguration, HasProtocolConstants,
                           HeaderHash, SlotId (..), difficultyL, epochIndexL, getChainDifficulty,
                           headerHash, headerSlotL)
import           Pos.Core.Txp (TxAux, TxUndo)
import           Pos.DB (MonadDBRead, SomeBatchOp (..))
import           Pos.Slotting (getSlotStart)
import           Pos.Txp (ProcessBlundsSettings (..), TxpBlock, TxpBlund, TxpGlobalApplyMode,
                          TxpGlobalRollbackMode, TxpGlobalSettings (..), applyBlocksWith,
                          blundToAuxNUndo, processBlunds, txpGlobalSettings)
import           Pos.Txp.Settings (IsNewEpochOperation)
import           Pos.Txp.Toil
import           Pos.Util.Chrono (NewestFirst (..))

import           Pos.BlockchainImporter.Configuration (HasPostGresDB, withPostGreTransactionM)
import           Pos.BlockchainImporter.Txp.Toil (BlockchainImporterExtraModifier (..),
                                                  EGlobalToilM, eApplyToil, eRollbackToil)

{-|
    Settings used for global transactions data processing used by blockchainImporter.
    The 2*k blocks rollbacked and re-applied on every new epoch don't impact on the postgresdb
-}
blockchainImporterTxpGlobalSettings :: (HasConfiguration, HasPostGresDB) => TxpGlobalSettings
blockchainImporterTxpGlobalSettings =
    -- verification is same
    txpGlobalSettings
    { tgsApplyBlocks = \isNewEpoch -> applyBlocksWith $ applySettings isNewEpoch
    , tgsRollbackBlocks = \isNewEpoch -> processBlunds (rollbackSettings isNewEpoch) . getNewestFirst
    , tgsApplyBlockModifier = withPostGreTransactionM
    , tgsRollbackBlockModifier = withPostGreTransactionM
    }

applySettings ::
       (TxpGlobalApplyMode ctx m, HasConfiguration, HasPostGresDB)
    => IsNewEpochOperation -> ProcessBlundsSettings () BlockchainImporterExtraModifier m
applySettings isNewEpoch =
    ProcessBlundsSettings
        { pbsProcessSingle = applySingle isNewEpoch
        , pbsCreateEnv = createEmptyEnv
        , pbsExtraOperations = emptyExtraOp
        , pbsIsRollback = False
        }

rollbackSettings ::
       (TxpGlobalRollbackMode m, HasConfiguration, MonadIO m, HasPostGresDB)
    => IsNewEpochOperation -> ProcessBlundsSettings () BlockchainImporterExtraModifier m
rollbackSettings isNewEpoch =
    ProcessBlundsSettings
        { pbsProcessSingle = rollbackSingle isNewEpoch
        , pbsCreateEnv = createEmptyEnv
        , pbsExtraOperations = emptyExtraOp
        , pbsIsRollback = True
        }

applySingle ::
       forall ctx m. (HasConfiguration, HasPostGresDB, TxpGlobalApplyMode ctx m)
    => IsNewEpochOperation -> TxpBlund -> m (EGlobalToilM ())
applySingle isNewEpoch txpBlund = do
    -- @TxpBlund@ is a block/blund with a reduced set of information required for
    -- transaction processing. We use it to determine at which slot did a transaction
    -- occur. TxpBlund has TxpBlock inside. If it's Left, it's a genesis block which
    -- doesn't contain transactions. It doesn't have a slot, only epoch, but you can
    -- use e. g. SlotId epoch minBound. If it's Right, you can use headerSlotL lens.
    --
    -- type TxpBlund = (TxpBlock, TxpUndo)
    -- type TxpBlock = ComponentBlock TxPayload

    let txpBlock = txpBlund ^. _1
    let (slotId, maybeBlockHeight) = getBlockSlotAndHeight txpBlock

    -- Get the timestamp from that information.
    mTxTimestamp <- getSlotStart slotId

    let (txAuxesAndUndos, _) = blundToAuxNUndoWHash txpBlund
    eApplyToil isNewEpoch mTxTimestamp txAuxesAndUndos maybeBlockHeight

rollbackSingle ::
       forall m. (HasConfiguration, HasPostGresDB, MonadIO m, MonadDBRead m)
    => IsNewEpochOperation -> TxpBlund -> m (EGlobalToilM ())
rollbackSingle isNewEpoch txpBlund =
  let txpBlock         = txpBlund ^. _1
      (_, maybeBlockHeight) = getBlockSlotAndHeight txpBlock
      txAuxesAndUndos  = blundToAuxNUndo txpBlund
  in eRollbackToil isNewEpoch txAuxesAndUndos maybeBlockHeight

createEmptyEnv :: Applicative m => Utxo -> [TxAux] -> m ()
createEmptyEnv _ _ = pure ()

emptyExtraOp :: BlockchainImporterExtraModifier -> SomeBatchOp
emptyExtraOp _ = mempty

-- Zip block's TxAuxes and also add block hash
blundToAuxNUndoWHash :: TxpBlund -> ([(TxAux, TxUndo)], HeaderHash)
blundToAuxNUndoWHash blund@(blk, _) =
    (blundToAuxNUndo blund, headerHash blk)

-- Returns no chain difficulty if the passed block is a genesis
getBlockSlotAndHeight :: HasProtocolConstants => TxpBlock -> (SlotId, Maybe BlockCount)
getBlockSlotAndHeight txpBlock = case txpBlock of
  ComponentBlockGenesis genesisBlock ->
      ( SlotId
            { siEpoch = genesisBlock ^. epochIndexL
            , siSlot  = minBound
            -- Genesis block doesn't have a slot, set to minBound
            }
      , Nothing
      )
  ComponentBlockMain mainHeader _  ->
      ( mainHeader ^. headerSlotL
      , -- Currently the chain difficulty is the number of the block, if that definition changes,
        -- this should also as well
        Just $ getChainDifficulty $ mainHeader ^. difficultyL
      )
