-- | Module containing blockchainImporter-specific datatypes

module Pos.BlockchainImporter.Core.Types
       ( TxExtra (..)
       ) where

import           Universum

import           Pos.Core (Timestamp)
import           Pos.Core.Txp (TxUndo)

data TxExtra = TxExtra
    { teFullProcessTime :: !(Maybe Timestamp)
    -- non-strict on purpose, see comment in `processTxDo` in Pos.BlockchainImporter.Txp.Local
    , teInputOutputs    :: TxUndo
    } deriving (Show, Generic, Eq)
