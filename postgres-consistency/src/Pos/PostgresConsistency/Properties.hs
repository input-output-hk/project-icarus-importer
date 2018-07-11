module Pos.PostgresConsistency.Properties
  (
    ConsistencyCheckerEnv
  , consistentBestBlock
  , consistentUtxo
  , allTxsStartingFromBlk
  , allTxsFromManyBlksFullfilProp
  , internalConsistentTxAddr
  ) where


import           Universum

import qualified Data.Map as Map
import qualified Database.PostgreSQL.Simple as PGS
import           Formatting (int, sformat, (%))
import           System.Wlog (WithLogger, logInfo)
import           UnliftIO (MonadUnliftIO)

import           Pos.BlockchainImporter.Configuration (HasPostGresDB, postGreOperate)
import qualified Pos.BlockchainImporter.Tables.BestBlockTable as BestBlkT (getBestBlock)
import qualified Pos.BlockchainImporter.Tables.TxsTable as TxsT (TxRecord, getTxByHash)
import qualified Pos.BlockchainImporter.Tables.UtxosTable as UtxosT (UtxoRecord (..), getUtxos)
import           Pos.Core (HasConfiguration, HasProtocolConstants, HeaderHash, getBlockCount,
                           getChainDifficulty)
import           Pos.Core.Block (mainBlockTxPayload)
import           Pos.Crypto (hash, hashHexF)
import           Pos.DB (MonadDBRead, getHeader, getMaxSeenDifficulty)
import           Pos.DB.Block (getBlock)
import           Pos.GState.BlockExtra (resolveForwardLink)
import           Pos.PostgresConsistency.Utils
import           Pos.Txp (Tx, TxAux (..), Utxo, flattenTxPayload)
import           Pos.Txp.DB (getAllPotentiallyHugeUtxo)

type ConsistencyCheckerEnv m =
  ( HasConfiguration
  , MonadDBRead m
  , MonadIO m
  , MonadUnliftIO m
  , WithLogger m
  , HasPostGresDB
  , HasProtocolConstants
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

allTxsStartingFromBlk ::
     ConsistencyCheckerEnv m
  => (Maybe TxsT.TxRecord -> Tx -> Bool)
  -> HeaderHash
  -> m Bool
allTxsStartingFromBlk prop initialHash = do
  logInfo "Checking consistency for all blocks starting from the given one"
  (allConsistent, totalChecked) <- allTxsFromBlkWithLogging 0 prop initialHash
  logInfo $ sformat ("Finished check: "%int%" blocks are consistent") totalChecked
  pure allConsistent
  where
    allTxsFromBlkWithLogging ::
         ConsistencyCheckerEnv m
      => Int
      -> (Maybe TxsT.TxRecord -> Tx -> Bool)
      -> HeaderHash
      -> m (Bool, Int)
    allTxsFromBlkWithLogging numberChecked txRowProp blkHash = do
      when (mod numberChecked 1000 == 0) $
        logInfo $ sformat ("Successfully checked "%int%" blocks with last hash: "%hashHexF)
                          numberChecked blkHash
      maybeT (getHeader blkHash) ( pure (True, numberChecked) ) $ \initialHeader -> do
        validTxsHistory <- allTxsFromBlkFullFilProp txRowProp blkHash
        if (not validTxsHistory) then pure (False, numberChecked)
        else maybeT (resolveForwardLink initialHeader) ( pure (True, numberChecked) ) $
                    allTxsFromBlkWithLogging (numberChecked + 1) txRowProp

allTxsFromManyBlksFullfilProp ::
     ConsistencyCheckerEnv m
  => (Maybe TxsT.TxRecord -> Tx -> Bool)
  -> [HeaderHash]
  -> m Bool
allTxsFromManyBlksFullfilProp txRowProp blkHashes = do
  logInfo "Checking consistency for given blk hashes"
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

allTxsFromBlkFullFilProp ::
     ConsistencyCheckerEnv m
  => (Maybe TxsT.TxRecord -> Tx -> Bool)
  -> HeaderHash
  -> m Bool
allTxsFromBlkFullFilProp txRowProp blkHash = do
  maybeT (getKVTxsByBlkHash blkHash) (pure False) $ \txs -> do
      allFullfilProp <- forM txs $ \(TxAux tx _) -> do
        let txHash = hash tx
        maybeTxPGS <- liftIO $ postGreOperate $ TxsT.getTxByHash txHash
        let fullfilsProp = txRowProp maybeTxPGS tx
        unless fullfilsProp $
               logInfo $ sformat ("Tx "%hashHexF%" prop check failed") txHash
        pure $ fullfilsProp
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

containsUtxo :: [UtxosT.UtxoRecord] -> Utxo -> Bool
containsUtxo pgUtxos kvUtxos = isNothing $ find (\row -> not $ hasUtxoRow row kvUtxos) pgUtxos

hasUtxoRow :: UtxosT.UtxoRecord -> Utxo -> Bool
hasUtxoRow (UtxosT.UtxoRecord pgTxIn pgTxOut) kvUtxos = isJust $ do
  kvOut <- Map.lookup pgTxIn kvUtxos
  if show kvOut == (show pgTxOut :: String) then Just ()
                                            else Nothing
  --FIXME: Why does tx out equality doesn't work?
