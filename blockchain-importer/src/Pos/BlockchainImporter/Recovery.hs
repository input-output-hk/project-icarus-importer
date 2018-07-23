module Pos.BlockchainImporter.Recovery
  ( -- * Recovery functions
    recoverDBsConsistency
  ) where

import           Universum

import           Control.Lens (_Wrapped)
import           Data.Map ((\\))
import           Formatting (build, int, sformat, (%))
import           JsonLog (CanJsonLog (..))
import           System.Wlog (logInfo, logWarning)

import           Pos.Block.Logic (BypassSecurityCheck (..), MonadBlockApply, rollbackBlocksUnsafe)
import           Pos.Block.Slog (ShouldCallBListener (..))
import           Pos.Block.Types (Blund)
import           Pos.BlockchainImporter.Configuration (HasPostGresDB, postGreOperate)
import qualified Pos.BlockchainImporter.Tables.BestBlockTable as BestBlockT
import qualified Pos.BlockchainImporter.Tables.TxsTable as TxsT
import qualified Pos.BlockchainImporter.Tables.UtxosTable as UtxosT
import           Pos.Core (ChainDifficulty, HasConfiguration, HasGeneratedSecrets,
                           HasGenesisBlockVersionData, HasGenesisData, HasProtocolConstants,
                           blkSecurityParam, difficultyL, epochIndexL)
import qualified Pos.DB.Block.Load as DB
import qualified Pos.DB.BlockIndex as DB
import           Pos.Ssc.Configuration (HasSscConfiguration)
import           Pos.StateLock (Priority (..), StateLock, StateLockMetrics, withStateLock)
import           Pos.Txp (Utxo, genesisUtxo, unGenesisUtxo, utxoToModifier)
import           Pos.Txp.DB (getAllPotentiallyHugeUtxo)
import           Pos.Util.Chrono (NewestFirst, _NewestFirst)
import           Pos.Util.JsonLog.Events (MemPoolModifyReason (..))
import           Pos.Util.Util (HasLens')

type RecoveryMode ctx m =
  ( MonadBlockApply ctx m
  , CanJsonLog m
  , HasLens' ctx StateLock
  , HasLens' ctx (StateLockMetrics MemPoolModifyReason)
  , HasConfiguration
  , HasPostGresDB
  , HasGeneratedSecrets
  , HasGenesisData
  , HasProtocolConstants
  , HasGenesisBlockVersionData
  )

{-|
    Recovers the consistency between the rocks and postgres db configured for the importer:
    1. Find the latest common block between the db, 'n'
    2. Rollback both db's to 'n - k' (with 'k' the maximum number of blocks that can be rollbacked)
       Note: Substracting 'k' is done as both dbs could be on different forks.
             The length of the fork can't be bigger than 'k', as defined in the Cardano protocol.
-}
recoverDBsConsistency :: RecoveryMode ctx m => m ()
recoverDBsConsistency = do
  logInfo "Recovery mode enabled! Starting recovery..."

  rocksBestBlock <- rocksDBTipDifficulty
  postgresBestBlock <- liftIO $ postGreOperate BestBlockT.getBestBlock
  logInfo $ sformat ("Rocks db's best block is "%build) rocksBestBlock
  logInfo $ sformat ("Postgres db's best block is "%build) postgresBestBlock
  let rollbackTo = blkNumberToRollback rocksBestBlock postgresBestBlock

  -- Note: Rollbacking order is important! Rocks db should be rollbacked before postgres db
  logInfo $ sformat ("Rollbacking both to "%build) rollbackTo
  rollbackRocksDB rollbackTo
  rollbackPostgresDB rollbackTo

  logInfo "Recovery finished!"


----------------------------------------------------------------------------
-- Rollbacking functions
----------------------------------------------------------------------------

rollbackRocksDB :: RecoveryMode ctx m => ChainDifficulty -> m ()
rollbackRocksDB rollbackTo = withStateLock HighPriority ApplyBlockWithRollback $ \_ -> do
  tipDifficulty <- rocksDBTipDifficulty
  let numToRollback = fromIntegral $ tipDifficulty - rollbackTo
  printTipDifficulty
  blundsMaybeEmpty <- modifyBlunds <$> DB.loadBlundsFromTipByDepth numToRollback
  -- NOTE: We may load more blunds than necessary, as genesis blocks don't
  --       contribute to depth counter.
  logInfo $ sformat ("Loaded "%int%" blunds") (length blundsMaybeEmpty)
  case _Wrapped nonEmpty blundsMaybeEmpty of
      Nothing -> pass
      Just blunds -> do
          -- ShouldCallBListener is False so that the rollbacks don't affect postgres db
          rollbackBlocksUnsafe (BypassSecurityCheck True) (ShouldCallBListener False) blunds
          logInfo $ sformat ("Rolled back "%int%" blocks") (length blunds)
          printTipDifficulty
  where
  -- It's illegal to rollback 0-th genesis block.
  modifyBlunds :: HasSscConfiguration => NewestFirst [] Blund -> NewestFirst [] Blund
  modifyBlunds = over _NewestFirst skip0thGenesis
  skip0thGenesis = filter (not . is0thGenesis)
  is0thGenesis :: Blund -> Bool
  is0thGenesis (Left genBlock, _)
      | genBlock ^. epochIndexL == 0 = True
  is0thGenesis _ = False
  printTipDifficulty = do
      tipDifficulty <- rocksDBTipDifficulty
      logInfo $ sformat ("Rocks tip's difficulty is "%build) tipDifficulty

{-
    Rollbacks the postgres db state to the corresponding to a given block 'rollbackTo':
    - Best block number is set to 'rollbackTo'
    - All txs from after 'rollbackTo' are deleted
    - Utxos are replaced with the ones from rocks db

    Requires: Rocks db should have the state corresponding to 'rollbackTo'
              This is normally achieved through executing 'rollbackRocksDB rollbackTo' first
    Warning:  Internal postgres db consistency is lost during this function execution
              Recovery should be re-done if canceled
-}
rollbackPostgresDB :: RecoveryMode ctx m => ChainDifficulty -> m ()
rollbackPostgresDB rollbackTo = do
  logInfo $ sformat ("Rollbacking postgres db to block "%build) rollbackTo
  logWarning $ toText $ "Recovery process should be re-done if this is canceled, " ++
                        "it might leave data inconsistent"

  -- Rocks db should have the state corresponding to 'rollbackTo'
  rocksBestBlock <- rocksDBTipDifficulty
  unless (rocksBestBlock == rollbackTo) $
         error $ sformat ("Rocks db state isn't at block "%build%"!") rollbackTo

  liftIO $ postGreOperate $ TxsT.deleteTxsAfterBlk rollbackTo
  liftIO $ postGreOperate $ BestBlockT.updateBestBlock rollbackTo

  liftIO $ postGreOperate $ UtxosT.clearUtxos
  kvUtxos <- nonGenesisRocksUtxos
  liftIO $ postGreOperate $ UtxosT.applyModifierToUtxos $ utxoToModifier kvUtxos

  logInfo $ sformat ("Rollbacked postgres db to block "%build) rollbackTo


----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

blkNumberToRollback ::
     HasProtocolConstants
  => ChainDifficulty        -- Rocks db best block number
  -> ChainDifficulty        -- Postgres db best block number
  -> ChainDifficulty
blkNumberToRollback rocksBestBlock postgresBestBlock =
  if latestCommonBlk > maxRollback then latestCommonBlk - maxRollback
                                   else 0
  where latestCommonBlk = min rocksBestBlock postgresBestBlock
        maxRollback = fromIntegral blkSecurityParam

nonGenesisRocksUtxos :: RecoveryMode ctx m => m Utxo
nonGenesisRocksUtxos = do
  allKVUtxos <- getAllPotentiallyHugeUtxo
  let genesisUtxos = unGenesisUtxo genesisUtxo
  pure $ allKVUtxos \\ genesisUtxos

rocksDBTipDifficulty :: RecoveryMode ctx m => m ChainDifficulty
rocksDBTipDifficulty = view difficultyL <$> DB.getTipHeader
