{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Module containing blockchainImporter-specific logic and data

module Pos.BlockchainImporter.DB
       ( BlockchainImporterOp (..)
       , getTxExtra
       , getAddrHistory
       , getAddrBalance
       , getUtxoSum
       , blockchainImporterInitDB
       , sanityCheckBalances
       ) where

import           Universum

import           Control.Lens (at, non)
import           Control.Monad.Trans.Resource (ResourceT)
import           Data.Conduit (ConduitT, mapOutput, runConduitRes, (.|))
import qualified Data.Conduit.List as CL
import qualified Database.RocksDB as Rocks
import           Formatting (sformat, (%))
import           Serokell.Util (Color (Red), colorize, mapJson)
import           System.Wlog (WithLogger, logError)
import           UnliftIO (MonadUnliftIO)

import           Pos.Binary.Class (serialize')
import           Pos.BlockchainImporter.Core (AddrHistory, TxExtra (..))
import           Pos.Core (Address, Coin, HasConfiguration, coinToInteger, unsafeAddCoin)
import           Pos.Core.Txp (TxId, TxOut (..), TxOutAux (..))
import           Pos.DB (DBError (..), DBIteratorClass (..), DBTag (GStateDB), MonadDB,
                         MonadDBRead (dbGet), RocksBatchOp (..), dbIterSource, dbSerializeValue,
                         encodeWithKeyPrefix)
import           Pos.DB.DB (initNodeDBs)
import           Pos.DB.GState.Common (gsGetBi, gsPutBi, writeBatchGState)
import           Pos.Ssc (HasSscConfiguration)
import           Pos.Txp.DB (getAllPotentiallyHugeUtxo, utxoSource)
import           Pos.Txp.GenesisUtxo (genesisUtxo)
import           Pos.Txp.Toil (GenesisUtxo (..), utxoF, utxoToAddressCoinPairs)
import           Pos.Util.Chrono (NewestFirst (..))
import           Pos.Util.Util (maybeThrow)



blockchainImporterInitDB
    :: forall ctx m.
       ( MonadReader ctx m
       , MonadUnliftIO m
       , MonadDB m
       , HasConfiguration
       , HasSscConfiguration
       )
    => m ()
blockchainImporterInitDB = initNodeDBs >> prepareBlockchainImporterDB

----------------------------------------------------------------------------
-- Getters
----------------------------------------------------------------------------

getTxExtra :: MonadDBRead m => TxId -> m (Maybe TxExtra)
getTxExtra = gsGetBi . txExtraPrefix

getAddrHistory :: MonadDBRead m => Address -> m AddrHistory
getAddrHistory = fmap (NewestFirst . concat . maybeToList) .
                 gsGetBi . addrHistoryKey

getAddrBalance :: MonadDBRead m => Address -> m (Maybe Coin)
getAddrBalance = gsGetBi . addrBalanceKey

getUtxoSum :: MonadDBRead m => m Integer
getUtxoSum = maybeThrow dbNotInitialized =<< gsGetBi utxoSumPrefix
  where
    dbNotInitialized = DBMalformed "getUtxoSum: DB is not initialized"

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

prepareBlockchainImporterDB :: (MonadDB m, MonadUnliftIO m, HasConfiguration) => m ()
prepareBlockchainImporterDB = do
    unlessM balancesInitializedM $ do
        let GenesisUtxo utxo = genesisUtxo
            addressCoinPairs = utxoToAddressCoinPairs utxo
        putGenesisBalances addressCoinPairs
        putInitFlag

    -- Smooth migration for CSE-228.
    unlessM utxoSumInitializedM $ do
        putCurrentUtxoSum


balancesInitFlag :: ByteString
balancesInitFlag = "e/init/"

balancesInitializedM :: MonadDBRead m => m Bool
balancesInitializedM = isJust <$> dbGet GStateDB balancesInitFlag

putInitFlag :: MonadDB m => m ()
putInitFlag = gsPutBi balancesInitFlag True

putGenesisBalances :: (MonadDB m, HasConfiguration) => [(Address, Coin)] -> m ()
putGenesisBalances addressCoinPairs = writeBatchGState putAddrBalancesOp
  where
    putAddrBalancesOp :: [BlockchainImporterOp]
    putAddrBalancesOp = map (uncurry PutAddrBalance) addressCoinPairs

utxoSumInitializedM :: MonadDBRead m => m Bool
utxoSumInitializedM = isJust <$> dbGet GStateDB utxoSumPrefix

putCurrentUtxoSum :: (MonadDB m, MonadUnliftIO m, HasConfiguration) => m ()
putCurrentUtxoSum = do
    utxoSum <- computeUtxoSum
    writeBatchGState [PutUtxoSum utxoSum]
  where
    computeUtxoSum :: (MonadDBRead m, MonadUnliftIO m) => m Integer
    computeUtxoSum = do
        let txOutValueSource =
                mapOutput (coinToInteger . txOutValue . toaOut . snd) utxoSource
        runConduitRes $ txOutValueSource .| CL.fold (+) 0

----------------------------------------------------------------------------
-- Batch operations
----------------------------------------------------------------------------

data BlockchainImporterOp
    = AddTxExtra !TxId !TxExtra
    | DelTxExtra !TxId

    | UpdateAddrHistory !Address !AddrHistory

    | PutAddrBalance !Address !Coin
    | DelAddrBalance !Address

    | PutUtxoSum !Integer

instance HasConfiguration => RocksBatchOp BlockchainImporterOp where
    toBatchOp (AddTxExtra id extra) =
        [Rocks.Put (txExtraPrefix id) (dbSerializeValue extra)]
    toBatchOp (DelTxExtra id) =
        [Rocks.Del $ txExtraPrefix id]

    toBatchOp (UpdateAddrHistory addr txs)
        | null txs = [Rocks.Del key]
        | otherwise = [Rocks.Put key (dbSerializeValue txs)]
      where
        key = addrHistoryKey addr

    toBatchOp (PutAddrBalance addr coin) =
        [Rocks.Put (addrBalanceKey addr) (dbSerializeValue coin)]
    toBatchOp (DelAddrBalance addr) =
        [Rocks.Del $ addrBalanceKey addr]

    toBatchOp (PutUtxoSum utxoSum) =
        [Rocks.Put utxoSumPrefix (dbSerializeValue utxoSum)]

----------------------------------------------------------------------------
-- Iteration
----------------------------------------------------------------------------

----------------------------------------------------------------------------
--- Balances
----------------------------------------------------------------------------

data BalancesIter

instance DBIteratorClass BalancesIter where
    type IterKey BalancesIter = Address
    type IterValue BalancesIter = Coin
    iterKeyPrefix = addrBalancePrefix

-- 'Source' corresponding to the whole balances mapping (for all addresses).
balancesSource :: (MonadDBRead m) => ConduitT () (Address, Coin) (ResourceT m) ()
balancesSource = dbIterSource GStateDB (Proxy @BalancesIter)

-- 'Sink' to turn balances source to a map.
balancesSink :: (MonadDBRead m) => ConduitT (Address, Coin) Void m (HashMap Address Coin)
balancesSink =
    CL.fold
        (\res (addr, coin) -> res & at addr . non minBound %~ unsafeAddCoin coin)
        mempty


----------------------------------------------------------------------------
-- Sanity check
----------------------------------------------------------------------------

-- | Check that balances stored in the BlockchainImporter DB are the same as the
-- balances computed from Utxo DB.
--
-- WARNING: this is potentially expensive operation, it shouldn't be
-- used in production.
sanityCheckBalances
    :: (MonadDBRead m, WithLogger m, MonadUnliftIO m)
    => m ()
sanityCheckBalances = do
    let utxoBalancesSource =
            mapOutput ((txOutAddress &&& txOutValue) . toaOut . snd) utxoSource
    storedMap <- runConduitRes $ balancesSource .| balancesSink
    computedFromUtxoMap <- runConduitRes $ utxoBalancesSource .| balancesSink
    let fmt =
            ("BlockchainImporter's balances are inconsistent with UTXO.\nBlockchainImporter stores: "
             %mapJson%".\nUtxo version is: "%mapJson%"\n")
    let msg = sformat fmt storedMap computedFromUtxoMap
    unless (storedMap == computedFromUtxoMap) $ do
        logError $ colorize Red msg
        logError . colorize Red . sformat ("Actual utxo is: " %utxoF) =<<
            getAllPotentiallyHugeUtxo
        throwM $ DBMalformed msg

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

txExtraPrefix :: TxId -> ByteString
txExtraPrefix h = "e/tx/" <> serialize' h

addrHistoryKey :: Address -> ByteString
addrHistoryKey addr = "e/ah/" <> serialize' addr

addrBalancePrefix :: ByteString
addrBalancePrefix = "e/ab/"

addrBalanceKey :: Address -> ByteString
addrBalanceKey = encodeWithKeyPrefix @BalancesIter

utxoSumPrefix :: ByteString
utxoSumPrefix = "e/utxosum/"
