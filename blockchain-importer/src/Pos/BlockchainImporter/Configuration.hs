-- | Configuration of PostGres DB.

{-# LANGUAGE RankNTypes #-}

module Pos.BlockchainImporter.Configuration
       ( HasPostGresDB
       , withPostGresDB
       , withPostGreTransaction
       , withPostGreTransactionM
       , maybePostGreStore
       , postGreOperate
       ) where

import           Universum

import           Data.Reflection (Given (..), give, given)
import qualified Database.PostgreSQL.Simple as PGS
import           UnliftIO (MonadUnliftIO, withRunInIO)

import           Pos.Core (BlockCount)

type HasPostGresDB = Given PostGresDBConfiguration

data PostGresDBConfiguration = PostGresDBConfiguration
    {
      pgConnection :: !PGS.Connection
      -- ^ Connection to PostGres DB
    , pgStartBlock :: !BlockCount
      -- ^ Starting block number from which data will be stored on the DB
    }

withPostGreTransaction :: HasPostGresDB => IO a -> IO a
withPostGreTransaction = PGS.withTransaction (pgConnection given)

withPostGreTransactionM :: forall m . (MonadUnliftIO m, MonadIO m, HasPostGresDB) => m () -> m ()
withPostGreTransactionM m = withRunInIO $ \runInIO -> withPostGreTransaction $ runInIO m

maybePostGreStore :: HasPostGresDB => BlockCount -> (PGS.Connection -> IO ()) -> IO ()
maybePostGreStore currBN storeFn
  | (fromIntegral currBN) >= (pgStartBlock given)  = postGreOperate storeFn
  | otherwise                                      = pure ()

postGreOperate :: HasPostGresDB => (PGS.Connection -> IO a) -> IO a
postGreOperate storeFn = storeFn $ pgConnection given

withPostGresDB :: PGS.Connection -> BlockCount -> (HasPostGresDB => r) -> r
withPostGresDB conn startBlock = give $ PostGresDBConfiguration conn startBlock
