-- | This module tests Binary instances for 'Pos.BlockchainImporter' types.

module Test.Pos.BlockchainImporter.Identity.BinarySpec
       ( spec
       ) where

import           Universum

import           Test.Hspec (Spec, describe)

import           Pos.Arbitrary.BlockchainImporter ()
import           Pos.BlockchainImporter.Core (TxExtra)
import           Test.Pos.Configuration (withDefConfiguration)
import           Test.Pos.Helpers (binaryTest)

spec :: Spec
spec = withDefConfiguration $ describe "BlockchainImporter types" $ do
    describe "Bi instances" $ do
        binaryTest @TxExtra
