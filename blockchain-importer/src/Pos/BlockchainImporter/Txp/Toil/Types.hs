-- | Additional types used by blockchainImporter's toil.

module Pos.BlockchainImporter.Txp.Toil.Types
       ( BlockchainImporterExtraModifier (..)
       , eemLocalTxsExtra
       ) where

import           Universum

import           Control.Lens (makeLenses)
import           Data.Default (Default, def)

import           Pos.BlockchainImporter.Core (TxExtra)
import           Pos.Core (TxId)
import qualified Pos.Util.Modifier as MM

type TxMapExtra = MM.MapModifier TxId TxExtra

-- | Modifier of extra data stored by blockchainImporter.
data BlockchainImporterExtraModifier = BlockchainImporterExtraModifier
    { _eemLocalTxsExtra :: !TxMapExtra
    }

makeLenses ''BlockchainImporterExtraModifier

instance Default BlockchainImporterExtraModifier where
    def = BlockchainImporterExtraModifier { _eemLocalTxsExtra = mempty }
