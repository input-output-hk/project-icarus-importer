-- | Arbitrary instances for PostgresConsistency types.

module Pos.Arbitrary.PostgresConsistency () where

import           Test.QuickCheck (Arbitrary (..))
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)

import           Pos.Core.Common (HeaderHash)
import           Pos.PostgresConsistency.Core.Types (TxExtra (..))
import           Pos.Txp ()

instance Arbitrary HeaderHash => Arbitrary TxExtra where
    arbitrary = genericArbitrary
    shrink = genericShrink
