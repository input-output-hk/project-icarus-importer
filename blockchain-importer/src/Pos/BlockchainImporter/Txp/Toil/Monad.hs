{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Monads used for blockchainImporter's toil.

module Pos.BlockchainImporter.Txp.Toil.Monad
       (
         BlockchainImporterExtraM

       , ELocalToilM
       , blockchainImporterExtraMToELocalToilM

       , EGlobalToilM
       , blockchainImporterExtraMToEGlobalToilM
       ) where

import           Universum

import           Control.Lens (magnify, zoom)
import           Control.Monad.Free.Church (F (..))
import           Control.Monad.Morph (generalize, hoist)
import           Control.Monad.Reader (mapReaderT)
import           Control.Monad.State.Strict (mapStateT)
import           System.Wlog (NamedPureLogger)

import           Pos.BlockchainImporter.Txp.Toil.Types (BlockchainImporterExtraModifier)
import           Pos.Txp.Toil (ExtendedGlobalToilM, ExtendedLocalToilM, StakesLookupF)
import           Pos.Util (type (~>))

----------------------------------------------------------------------------
-- Monadic actions with extra txp data.
----------------------------------------------------------------------------

-- | Utility monad which allows to lookup extra values related to txp and modify them.
type BlockchainImporterExtraM
     = ReaderT () (StateT BlockchainImporterExtraModifier (NamedPureLogger Identity))

----------------------------------------------------------------------------
-- Monad used for local Toil in BlockchainImporter.
----------------------------------------------------------------------------

type ELocalToilM = ExtendedLocalToilM () BlockchainImporterExtraModifier

blockchainImporterExtraMToELocalToilM :: BlockchainImporterExtraM ~> ELocalToilM
blockchainImporterExtraMToELocalToilM = zoom _2 . magnify _2

----------------------------------------------------------------------------
-- Monad used for global Toil in BlockchainImporter.
----------------------------------------------------------------------------

type EGlobalToilM
     = ExtendedGlobalToilM () BlockchainImporterExtraModifier

blockchainImporterExtraMToEGlobalToilM :: BlockchainImporterExtraM ~> EGlobalToilM
blockchainImporterExtraMToEGlobalToilM = mapReaderT (mapStateT f . zoom _2) . magnify _2
  where
    f :: NamedPureLogger Identity ~> NamedPureLogger (F StakesLookupF)
    f = hoist generalize
