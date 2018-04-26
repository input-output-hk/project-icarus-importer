-- Classes and datatypes for managing extra context in BlockchainImporter.

{-# LANGUAGE RankNTypes #-}

module Pos.BlockchainImporter.ExtraContext
    ( ExtraContext (..)
    , ExtraContextT
    , runExtraContextT
    , makeExtraCtx

    , HasGenesisRedeemAddressInfo (..)
    , GenesisRedeemAddressInfo
    -- * Genesis address info

    , HasBlockchainImporterCSLInterface (..)
    , BlockchainImporterMockableMode (..)
    , makeMockExtraCtx
    -- * BlockchainImporter mock interface

    ) where

import           Universum

import qualified Data.Vector as V
import qualified Ether

import           Data.Default (Default (..), def)
import           Pos.Block.Types (Blund)
import           Pos.Core.Block (Block)
import qualified Pos.DB.Block as DB
import           Pos.DB.Class (MonadDBRead)

import           Pos.BlockchainImporter.DB (Epoch, Page, getEpochBlocks, getEpochPages, getPageBlocks)

import           Pos.Core (Address, Coin, EpochIndex, HasConfiguration, HeaderHash, SlotId (..),
                           SlotLeaders, Timestamp, isRedeemAddress)
import           Pos.Lrc (getLeadersForEpoch)
import           Pos.Slotting (MonadSlotsData, getSlotStart)
import           Pos.Txp (GenesisUtxo (..), genesisUtxo, utxoToAddressCoinPairs)


-------------------------------------------------------------------------------------
-- Extra context
-------------------------------------------------------------------------------------

type ExtraContextT m = Ether.ReaderT' ExtraContext m

runExtraContextT :: Monad m => ExtraContext -> ExtraContextT m a -> m a
runExtraContextT = flip Ether.runReaderT

data ExtraContext = ExtraContext
    { ecAddressCoinPairs     :: !GenesisRedeemAddressInfo
    , ecBlockchainImporterMockableMode :: !BlockchainImporterMockableMode
    }

makeExtraCtx :: HasConfiguration => ExtraContext
makeExtraCtx =
    let addressCoinPairs = utxoToAddressCoinPairs $ unGenesisUtxo genesisUtxo
        redeemOnly = filter (isRedeemAddress . fst) addressCoinPairs
    in ExtraContext
        { ecAddressCoinPairs     = V.fromList redeemOnly
        , ecBlockchainImporterMockableMode = prodMode
        }

-- | For mocking we mostly need to replace just the external CSL functions.
makeMockExtraCtx :: HasConfiguration => BlockchainImporterMockableMode -> ExtraContext
makeMockExtraCtx blockchainImporterMockMode =
    ExtraContext
        { ecAddressCoinPairs = V.empty
        , ecBlockchainImporterMockableMode = blockchainImporterMockMode
        }

-------------------------------------------------------------------------------------
-- Genesis redeem address info
-------------------------------------------------------------------------------------

type GenesisRedeemAddressInfo = V.Vector (Address, Coin)

class HasGenesisRedeemAddressInfo m where
    getGenesisRedeemAddressInfo :: m GenesisRedeemAddressInfo

instance Monad m => HasGenesisRedeemAddressInfo (ExtraContextT m) where
    getGenesisRedeemAddressInfo = do
        extraCtx <- Ether.ask @ExtraContext
        pure $ ecAddressCoinPairs extraCtx

-------------------------------------------------------------------------------------
-- BlockchainImporter mock mode
--
-- The simple data structure that encapsulates functions that use CSL. We want to "cut"
-- them out of the picture in order to be able to mock them.
-------------------------------------------------------------------------------------

data BlockchainImporterMockableMode = BlockchainImporterMockableMode
    { emmGetTipBlock
          :: forall m. MonadDBRead m => m Block
    , emmGetPageBlocks
          :: forall m. MonadDBRead m => Page -> m (Maybe [HeaderHash])
    , emmGetBlundFromHH
          :: forall m. MonadDBRead m => HeaderHash -> m (Maybe Blund)
    , emmGetSlotStart
          :: forall ctx m. MonadSlotsData ctx m => SlotId -> m (Maybe Timestamp)
    , emmGetLeadersFromEpoch
          :: forall m. MonadDBRead m => EpochIndex -> m (Maybe SlotLeaders)
    , emmGetEpochBlocks
          :: forall m. MonadDBRead m => Epoch -> Page -> m (Maybe [HeaderHash])
    , emmGetEpochPages
          :: forall m. MonadDBRead m => Epoch -> m (Maybe Page)
    }

-- | This is what we use in production when we run BlockchainImporter.
prodMode :: BlockchainImporterMockableMode
prodMode = BlockchainImporterMockableMode {
      emmGetTipBlock            = DB.getTipBlock,
      emmGetPageBlocks          = getPageBlocks,
      emmGetBlundFromHH         = DB.getBlund,
      emmGetSlotStart           = getSlotStart,
      emmGetLeadersFromEpoch    = getLeadersForEpoch,
      emmGetEpochBlocks         = getEpochBlocks,
      emmGetEpochPages          = getEpochPages
    }

-- | So we can just reuse the default instance and change individial functions.
-- On one side, it removes the compile error(s) for having all functions implemented.
-- On the other side, it moves that error into runtime and enables simple mocking.
-- This is a good thing once we have a larger amount of functions, like in _blockchainImporter_,
-- and this gives us the flexibility to "mock" whichever we want.
instance Default (BlockchainImporterMockableMode) where
  def = BlockchainImporterMockableMode {
        emmGetTipBlock            = errorImpl,
        emmGetPageBlocks          = errorImpl,
        emmGetBlundFromHH         = errorImpl,
        emmGetSlotStart           = errorImpl,
        emmGetLeadersFromEpoch    = errorImpl,
        emmGetEpochBlocks         = errorImpl,
        emmGetEpochPages          = errorImpl
      }
    where
      errorImpl = error "Cannot be used, please implement this function!"


-------------------------------------------------------------------------------------
-- BlockchainImporter interface instance
-------------------------------------------------------------------------------------

-- | We use this for an external CSL functions representation so we can mock them when
-- testing.
class HasBlockchainImporterCSLInterface m where
    getTipBlockCSLI :: m Block
    getPageBlocksCSLI :: Page -> m (Maybe [HeaderHash])
    getBlundFromHHCSLI :: HeaderHash -> m (Maybe Blund)
    getSlotStartCSLI :: SlotId -> m (Maybe Timestamp)
    getLeadersFromEpochCSLI :: EpochIndex -> m (Maybe SlotLeaders)
    getEpochBlocksCSLI :: Epoch -> Page -> m (Maybe [HeaderHash])
    getEpochPagesCSLI :: Epoch -> m (Maybe Page)

-- | The instance for external CSL functions.
instance (Monad m, MonadDBRead m, MonadSlotsData ctx m) =>
    HasBlockchainImporterCSLInterface (ExtraContextT m) where

    getTipBlockCSLI = do
        extraCtx <- Ether.ask @ExtraContext
        let blockchainImporterMockMode = ecBlockchainImporterMockableMode extraCtx
        emmGetTipBlock blockchainImporterMockMode

    getPageBlocksCSLI page = do
        extraCtx <- Ether.ask @ExtraContext
        let blockchainImporterMockMode = ecBlockchainImporterMockableMode extraCtx
        emmGetPageBlocks blockchainImporterMockMode page

    getBlundFromHHCSLI headerHash = do
        extraCtx <- Ether.ask @ExtraContext
        let blockchainImporterMockMode = ecBlockchainImporterMockableMode extraCtx
        emmGetBlundFromHH blockchainImporterMockMode headerHash

    getSlotStartCSLI slotId = do
        extraCtx <- Ether.ask @ExtraContext
        let blockchainImporterMockMode = ecBlockchainImporterMockableMode extraCtx
        emmGetSlotStart blockchainImporterMockMode slotId

    getLeadersFromEpochCSLI epochIndex = do
        extraCtx <- Ether.ask @ExtraContext
        let blockchainImporterMockMode = ecBlockchainImporterMockableMode extraCtx
        emmGetLeadersFromEpoch blockchainImporterMockMode epochIndex

    getEpochBlocksCSLI epoch page = do
        extraCtx <- Ether.ask @ExtraContext
        let blockchainImporterMockMode = ecBlockchainImporterMockableMode extraCtx
        emmGetEpochBlocks blockchainImporterMockMode epoch page

    getEpochPagesCSLI epoch = do
        extraCtx <- Ether.ask @ExtraContext
        let blockchainImporterMockMode = ecBlockchainImporterMockableMode extraCtx
        emmGetEpochPages blockchainImporterMockMode epoch

