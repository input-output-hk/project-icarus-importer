module Pos.PostgresConsistency.Properties
  (
    ConsistencyCheckerEnv
  , consistentBestBlock
  , consistentUtxo
  , allTxsFromManyBlksFullfilProp
  , internalConsistentTxAddr
  ) where


import           Universum

import qualified Data.Map as Map
import qualified Database.PostgreSQL.Simple as PGS
import           System.Wlog (WithLogger, logInfo)
import           UnliftIO (MonadUnliftIO)

import           Pos.BlockchainImporter.Configuration (HasPostGresDB, postGreOperate)
import qualified Pos.BlockchainImporter.Tables.BestBlockTable as BestBlkT (getBestBlock)
import qualified Pos.BlockchainImporter.Tables.TxsTable as TxsT (TxRow, getTxByHash)
import qualified Pos.BlockchainImporter.Tables.UtxosTable as UtxosT (UtxoRow, getUtxos)
import           Pos.Core (HasConfiguration, HeaderHash, getBlockCount, getChainDifficulty)
import           Pos.Core.Block (mainBlockTxPayload)
import           Pos.Crypto (hash)
import           Pos.DB (MonadDBRead, getMaxSeenDifficulty)
import           Pos.DB.Block (getBlock)
import           Pos.PostgresConsistency.Utils
import           Pos.Txp (TxAux (..), Utxo, flattenTxPayload)
import           Pos.Txp.DB (getAllPotentiallyHugeUtxo)

type ConsistencyCheckerEnv m =
  ( HasConfiguration
  , MonadDBRead m
  , MonadIO m
  , MonadUnliftIO m
  , WithLogger m
  , HasPostGresDB
  )


----------------------------------------------------------------------------
-- Properties
----------------------------------------------------------------------------

consistentBestBlock :: ConsistencyCheckerEnv m => m Bool
consistentBestBlock = do
  logInfo "Checking best block consistency"
  kvBestBlock <- getKVBestBlockNum
  pgBestBlock <- liftIO $ postGreOperate $ BestBlkT.getBestBlock
  let isConsistent = kvBestBlock == fromIntegral pgBestBlock
  logInfo $ toText $ if isConsistent  then "Best block is consistent"
                                      else "Best block is inconsistent: " ++
                                            "kv best block " ++ show kvBestBlock ++ "," ++
                                            "pg best block " ++ show pgBestBlock
  pure isConsistent

--FIXME: Genesis utxo not in pgUtxo, how to compare? (genesisUtxo :: HasGenesisData => GenesisUtxo)
consistentUtxo :: ConsistencyCheckerEnv m => m Bool
consistentUtxo = do
  logInfo "Checking utxo consistency"
  kvUtxos <- getAllPotentiallyHugeUtxo
  pgUtxos <- liftIO $ postGreOperate $ UtxosT.getUtxos
  let isConsistent = containsUtxo pgUtxos kvUtxos
  logInfo $ if isConsistent then "Utxo is consistent" else "Utxo is inconsistent"
  pure isConsistent

allTxsFromManyBlksFullfilProp ::
     ConsistencyCheckerEnv m
  => (Maybe TxsT.TxRow -> Bool)
  -> [HeaderHash]
  -> m Bool
allTxsFromManyBlksFullfilProp txRowProp blkHashes = do
  logInfo "Checking blk hashes consistency"
  eachFullFillProp <- mapM (allTxsFromBlkFullFilProp txRowProp) blkHashes
  let allFullfilProp = and eachFullFillProp
  logInfo $ if allFullfilProp then "blk's txs fullfil property"
                              else "blk's txs don't fullfil property"
  pure allFullfilProp

internalConsistentTxAddr :: ConsistencyCheckerEnv m => m Bool
internalConsistentTxAddr = do
  logInfo "Checking tx_addresses internal consistency"
  numberInconsistencies :: [PGS.Only Int64] <- liftIO $ postGreOperate $ (flip PGS.query_) txAddrConsistencyQuery
  let isConsistent = numberInconsistencies == [PGS.Only 0]
  logInfo $ if isConsistent then "tx_addresses table is consistent"
                            else "tx_addresses table is inconsistent"
  pure isConsistent
  where txAddrConsistencyQuery = fromString (
          "SELECT COUNT (*)" ++
          "  FROM" ++
          "  tx_addresses" ++
          "  FULL OUTER JOIN" ++
          "  (SELECT txs.hash as tx_hash, address FROM txs, unnest(txs.inputs_address) address" ++
          "  union all" ++
          "  SELECT txs.hash as tx_hash, address FROM txs, unnest(txs.outputs_address) address) as calcTxAddr" ++
          "    using (tx_hash, address)" ++
          "  WHERE calcTxAddr.tx_hash IS NULL OR tx_addresses.tx_hash IS NULL")


----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- FIXME: Log blk hashes that failed
allTxsFromBlkFullFilProp ::
     ConsistencyCheckerEnv m
  => (Maybe TxsT.TxRow -> Bool)
  -> HeaderHash
  -> m Bool
allTxsFromBlkFullFilProp txRowProp blkHash = do
  maybeT (getKVTxsByBlkHash blkHash) (pure False) $ \txs -> do
      allFullfilProp <- forM txs $ \(TxAux tx _) -> do
        let txHash = hash tx
        maybeTxPGS <- liftIO $ postGreOperate $ TxsT.getTxByHash txHash
        pure $ txRowProp maybeTxPGS
      pure $ and allFullfilProp

getKVTxsByBlkHash
  :: ConsistencyCheckerEnv m
  => HeaderHash -> m (Maybe [TxAux])
getKVTxsByBlkHash blkHash = do
  maybeBlock <- getBlock blkHash
  case maybeBlock of
    Just (Right blk) ->
      pure $ Just $ flattenTxPayload $ blk ^. mainBlockTxPayload
    Just (Left _) -> pure $ Just []
    Nothing -> pure $ Nothing

getKVBestBlockNum :: ConsistencyCheckerEnv m => m Word64
getKVBestBlockNum = getBlockCount . getChainDifficulty <$> getMaxSeenDifficulty

containsUtxo :: [UtxosT.UtxoRow] -> Utxo -> Bool
containsUtxo pgUtxos kvUtxos = isNothing $ find (\row -> not $ hasUtxoRow row kvUtxos) pgUtxos

hasUtxoRow :: UtxosT.UtxoRow -> Utxo -> Bool
hasUtxoRow (txHash, idx, receiver, amount) kvUtxos = isJust $ do
  pgTxIn <- toTxIn txHash idx
  pgTxOut <- toTxOut receiver amount
  kvOut <- Map.lookup pgTxIn kvUtxos
  if show kvOut == (show pgTxOut :: String) then Just ()
                                            else Nothing
  --FIXME: Why does tx out equality doesn't work?
