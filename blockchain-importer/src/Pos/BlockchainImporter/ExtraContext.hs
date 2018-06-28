-- Classes and datatypes for managing extra context in BlockchainImporter.

{-# LANGUAGE RankNTypes #-}

module Pos.BlockchainImporter.ExtraContext
    ( ExtraContext (..)
    , ExtraContextT
    , runExtraContextT
    , makeExtraCtx

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
import           Pos.Core.Block (Block)
import qualified Pos.DB.Block as DB
import           Pos.DB.Class (MonadDBRead)

import           Pos.Core (Address, Coin, HasConfiguration, isRedeemAddress)
import           Pos.Slotting (MonadSlotsData)
import           Pos.Txp (GenesisUtxo (..), genesisUtxo, utxoToAddressCoinPairs)


-------------------------------------------------------------------------------------
-- Extra context
-------------------------------------------------------------------------------------

type ExtraContextT m = Ether.ReaderT' ExtraContext m

runExtraContextT :: Monad m => ExtraContext -> ExtraContextT m a -> m a
runExtraContextT = flip Ether.runReaderT

data ExtraContext = ExtraContext
    { ecAddressCoinPairs               :: !GenesisRedeemAddressInfo
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

-------------------------------------------------------------------------------------
-- BlockchainImporter mock mode
--
-- The simple data structure that encapsulates functions that use CSL. We want to "cut"
-- them out of the picture in order to be able to mock them.
-------------------------------------------------------------------------------------

data BlockchainImporterMockableMode = BlockchainImporterMockableMode
    { emmGetTipBlock
          :: forall m. MonadDBRead m => m Block
    }

-- | This is what we use in production when we run BlockchainImporter.
prodMode :: BlockchainImporterMockableMode
prodMode = BlockchainImporterMockableMode {
      emmGetTipBlock            = DB.getTipBlock
    }

-- | So we can just reuse the default instance and change individial functions.
-- On one side, it removes the compile error(s) for having all functions implemented.
-- On the other side, it moves that error into runtime and enables simple mocking.
-- This is a good thing once we have a larger amount of functions, like in _blockchainImporter_,
-- and this gives us the flexibility to "mock" whichever we want.
instance Default (BlockchainImporterMockableMode) where
  def = BlockchainImporterMockableMode {
        emmGetTipBlock            = errorImpl
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

-- | The instance for external CSL functions.
instance (Monad m, MonadDBRead m, MonadSlotsData ctx m) =>
    HasBlockchainImporterCSLInterface (ExtraContextT m) where

    getTipBlockCSLI = do
        extraCtx <- Ether.ask @ExtraContext
        let blockchainImporterMockMode = ecBlockchainImporterMockableMode extraCtx
        emmGetTipBlock blockchainImporterMockMode

