-- | Configuration of PostGres DB.

{-# LANGUAGE RankNTypes #-}

module Pos.BlockchainImporter.Configuration
       ( HasPostGresDB
       , withPostGresDB
       , postGresDB
       ) where

import           Data.Reflection (Given (..), give, given)
import qualified Database.PostgreSQL.Simple as PGS

type HasPostGresDB = Given PGS.Connection

postGresDB :: HasPostGresDB => PGS.Connection
postGresDB = given

withPostGresDB :: PGS.Connection -> (HasPostGresDB => r) -> r
withPostGresDB = give
