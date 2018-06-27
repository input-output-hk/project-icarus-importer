-- | This module tests Binary instances for 'Pos.PostgresConsistency' types.

module Test.Pos.PostgresConsistency.Identity.BinarySpec
       ( spec
       ) where

import           Universum

import           Test.Hspec (Spec, describe)

import           Pos.Arbitrary.PostgresConsistency ()
import           Pos.PostgresConsistency.Core (TxExtra)
import           Test.Pos.Configuration (withDefConfiguration)
import           Test.Pos.Helpers (binaryTest)

spec :: Spec
spec = withDefConfiguration $ describe "PostgresConsistency types" $ do
    describe "Bi instances" $ do
        binaryTest @TxExtra
