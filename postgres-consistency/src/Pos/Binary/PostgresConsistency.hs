-- | Binary instances for blockchainImporter types

module Pos.Binary.PostgresConsistency () where

import           Universum

import           Pos.Binary ()
import           Pos.Binary.Class (Cons (..), Field (..), deriveSimpleBi)
import           Pos.Core (HeaderHash, Timestamp, TxUndo)
import           Pos.PostgresConsistency.Core.Types (TxExtra (..))

deriveSimpleBi ''TxExtra [
    Cons 'TxExtra [
        Field [| teBlockchainPlace :: Maybe (HeaderHash, Word32) |],
        Field [| teReceivedTime    :: Maybe Timestamp            |],
        Field [| teInputOutputs    :: TxUndo                     |]
    ]]
