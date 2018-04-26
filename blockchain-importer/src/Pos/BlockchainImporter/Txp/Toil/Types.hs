-- | Additional types used by blockchainImporter's toil.

module Pos.BlockchainImporter.Txp.Toil.Types
       ( BlockchainImporterExtraModifier (..)
       , eemLocalTxsExtra
       , eemAddrHistories
       , eemAddrBalances
       , eemNewUtxoSum
       , BlockchainImporterExtraLookup (..)
       ) where

import           Universum

import           Control.Lens (makeLenses)
import           Data.Default (Default, def)

import           Pos.Core (Address, Coin, TxId)
import           Pos.BlockchainImporter.Core (AddrHistory, TxExtra)
import qualified Pos.Util.Modifier as MM

type TxMapExtra = MM.MapModifier TxId TxExtra
type UpdatedAddrHistories = HashMap Address AddrHistory
type TxMapBalances = MM.MapModifier Address Coin

-- | Modifier of extra data stored by blockchainImporter.
data BlockchainImporterExtraModifier = BlockchainImporterExtraModifier
    { _eemLocalTxsExtra :: !TxMapExtra
    , _eemAddrHistories :: !UpdatedAddrHistories
    , _eemAddrBalances  :: !TxMapBalances
    , _eemNewUtxoSum    :: !(Maybe Integer)
    }

makeLenses ''BlockchainImporterExtraModifier

instance Default BlockchainImporterExtraModifier where
    def =
        BlockchainImporterExtraModifier
        { _eemLocalTxsExtra = mempty
        , _eemAddrHistories = mempty
        , _eemAddrBalances  = mempty
        , _eemNewUtxoSum    = Nothing
        }

-- | Functions to get extra data stored by blockchainImporter.
data BlockchainImporterExtraLookup = BlockchainImporterExtraLookup
    { eelGetTxExtra     :: TxId -> Maybe TxExtra
    , eelGetAddrHistory :: Address -> AddrHistory
    , eelGetAddrBalance :: Address -> Maybe Coin
    , eelGetUtxoSum     :: !Integer
    }
