{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Monads used for blockchainImporter's toil.

module Pos.BlockchainImporter.Txp.Toil.Monad
       (
         BlockchainImporterExtraM

       , getTxExtra
       , getAddrHistory
       , getAddrBalance
       , getUtxoSum

       , putTxExtra
       , delTxExtra
       , updateAddrHistory
       , putAddrBalance
       , delAddrBalance
       , putUtxoSum

       , ELocalToilM
       , blockchainImporterExtraMToELocalToilM

       , EGlobalToilM
       , blockchainImporterExtraMToEGlobalToilM
       ) where

import           Universum

import           Control.Lens (at, magnify, zoom, (%=), (.=))
import           Control.Monad.Free.Church (F (..))
import           Control.Monad.Morph (generalize, hoist)
import           Control.Monad.Reader (mapReaderT)
import           Control.Monad.State.Strict (mapStateT)
import           System.Wlog (NamedPureLogger)

import           Pos.Core (Address, Coin, TxId)
import           Pos.BlockchainImporter.Core (AddrHistory, TxExtra)
import           Pos.BlockchainImporter.Txp.Toil.Types (BlockchainImporterExtraLookup (..), BlockchainImporterExtraModifier,
                                              eemAddrBalances, eemAddrHistories, eemLocalTxsExtra,
                                              eemNewUtxoSum)
import           Pos.Txp.Toil (ExtendedGlobalToilM, ExtendedLocalToilM, StakesLookupF)
import qualified Pos.Util.Modifier as MM
import           Pos.Util (type (~>))

----------------------------------------------------------------------------
-- Monadic actions with extra txp data.
----------------------------------------------------------------------------

-- | Utility monad which allows to lookup extra values related to txp and modify them.
type BlockchainImporterExtraM
     = ReaderT BlockchainImporterExtraLookup (StateT BlockchainImporterExtraModifier (NamedPureLogger Identity))

getTxExtra :: TxId -> BlockchainImporterExtraM (Maybe TxExtra)
getTxExtra txId = do
    baseLookup <- eelGetTxExtra <$> ask
    MM.lookup baseLookup txId <$> use eemLocalTxsExtra

getAddrHistory :: Address -> BlockchainImporterExtraM AddrHistory
getAddrHistory addr = do
    use (eemAddrHistories . at addr) >>= \case
        Nothing -> eelGetAddrHistory <$> ask <*> pure addr
        Just hist -> pure hist

getAddrBalance :: Address -> BlockchainImporterExtraM (Maybe Coin)
getAddrBalance addr = do
    baseLookup <- eelGetAddrBalance <$> ask
    MM.lookup baseLookup addr <$> use eemAddrBalances

getUtxoSum :: BlockchainImporterExtraM Integer
getUtxoSum = fromMaybe <$> (eelGetUtxoSum <$> ask) <*> use eemNewUtxoSum

putTxExtra :: TxId -> TxExtra -> BlockchainImporterExtraM ()
putTxExtra txId extra = eemLocalTxsExtra %= MM.insert txId extra

delTxExtra :: TxId -> BlockchainImporterExtraM ()
delTxExtra txId = eemLocalTxsExtra %= MM.delete txId

updateAddrHistory :: Address -> AddrHistory -> BlockchainImporterExtraM ()
updateAddrHistory addr hist = eemAddrHistories . at addr .= Just hist

putAddrBalance :: Address -> Coin -> BlockchainImporterExtraM ()
putAddrBalance addr coin = eemAddrBalances %= MM.insert addr coin

delAddrBalance :: Address -> BlockchainImporterExtraM ()
delAddrBalance addr = eemAddrBalances %= MM.delete addr

putUtxoSum :: Integer -> BlockchainImporterExtraM ()
putUtxoSum utxoSum = eemNewUtxoSum .= Just utxoSum

----------------------------------------------------------------------------
-- Monad used for local Toil in BlockchainImporter.
----------------------------------------------------------------------------

type ELocalToilM = ExtendedLocalToilM BlockchainImporterExtraLookup BlockchainImporterExtraModifier

blockchainImporterExtraMToELocalToilM :: BlockchainImporterExtraM ~> ELocalToilM
blockchainImporterExtraMToELocalToilM = zoom _2 . magnify _2

----------------------------------------------------------------------------
-- Monad used for global Toil in BlockchainImporter.
----------------------------------------------------------------------------

type EGlobalToilM
     = ExtendedGlobalToilM BlockchainImporterExtraLookup BlockchainImporterExtraModifier

blockchainImporterExtraMToEGlobalToilM :: BlockchainImporterExtraM ~> EGlobalToilM
blockchainImporterExtraMToEGlobalToilM = mapReaderT (mapStateT f . zoom _2) . magnify _2
  where
    f :: NamedPureLogger Identity ~> NamedPureLogger (F StakesLookupF)
    f = hoist generalize
