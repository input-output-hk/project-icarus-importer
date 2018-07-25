-- | Configuration of PostGres DB.

{-# LANGUAGE RankNTypes #-}

module Pos.BlockchainImporter.Configuration
       ( HasPostGresDB
       , withPostGresDB
       , withPostGreTransaction
       , withPostGreTransactionM
       , postGreOperate
       ) where

import           Universum

import           Data.Reflection (Given (..), give, given)
import qualified Database.PostgreSQL.Simple as PGS
import           UnliftIO (MonadUnliftIO, withRunInIO)

type HasPostGresDB = Given PGS.Connection

withPostGreTransaction :: HasPostGresDB => IO a -> IO a
withPostGreTransaction = PGS.withTransaction given

withPostGreTransactionM :: forall m . (MonadUnliftIO m, MonadIO m, HasPostGresDB) => m () -> m ()
withPostGreTransactionM m = withRunInIO $ \runInIO -> withPostGreTransaction $ runInIO m

postGreOperate :: HasPostGresDB => (PGS.Connection -> IO a) -> IO a
postGreOperate storeFn = storeFn given

withPostGresDB :: PGS.Connection -> (HasPostGresDB => r) -> r
withPostGresDB = give
