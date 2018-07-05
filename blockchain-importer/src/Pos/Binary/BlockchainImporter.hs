-- | Binary instances for blockchainImporter types

module Pos.Binary.BlockchainImporter () where

import           Universum

import           Pos.Binary ()
import           Pos.Binary.Class (Cons (..), Field (..), deriveSimpleBi)
import           Pos.BlockchainImporter.Core.Types (TxExtra (..))
import           Pos.Core (Timestamp, TxUndo)

deriveSimpleBi ''TxExtra [
    Cons 'TxExtra [
        Field [| teFullProcessTime  :: Maybe Timestamp            |],
        Field [| teInputOutputs     :: TxUndo                     |]
    ]]
