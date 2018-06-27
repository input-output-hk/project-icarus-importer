{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}

-- | Blockchain listener for PostgresConsistency.
-- Callbacks on application and rollback.

module Pos.PostgresConsistency.BListener
       ( runBlockchainImporterBListener
       , BlockchainImporterBListener
       ) where

import           Universum

import           Control.Monad.Trans.Identity (IdentityT (..))
import           Data.Coerce (coerce)
import qualified Ether
import           System.Wlog (WithLogger)
import           UnliftIO (MonadUnliftIO)

import           Pos.Block.BListener (MonadBListener (..))
import           Pos.Core (HasConfiguration)
import           Pos.DB.Class (MonadDBRead)


----------------------------------------------------------------------------
-- Declarations
----------------------------------------------------------------------------


data BlockchainImporterBListenerTag

type BlockchainImporterBListener = Ether.TaggedTrans BlockchainImporterBListenerTag IdentityT

-- Unwrap the @BListener@.
runBlockchainImporterBListener :: BlockchainImporterBListener m a -> m a
runBlockchainImporterBListener = coerce

-- PostgresConsistency implementation for usual node. Combines the operations.
instance ( MonadDBRead m
         , MonadUnliftIO m
         , MonadCatch m
         , WithLogger m
         , HasConfiguration
         )
         => MonadBListener (BlockchainImporterBListener m) where
    onApplyBlocks     _ = pure mempty
    onRollbackBlocks  _ = pure mempty
