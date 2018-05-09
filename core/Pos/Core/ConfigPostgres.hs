-- FIXME: Move to blockchain-importer package
-- | Configuration of Postgres DB.

module Pos.Core.ConfigPostgres
       ( HasPostGresDB
       , withPostGresDB
       , postGresDB
       ) where

import           Universum

import           Data.Reflection (Given (..), give, given)
import qualified Database.PostgreSQL.Simple as PGS

type HasPostGresDB = Given PGS.Connection

postGresDB :: HasPostGresDB => PGS.Connection
postGresDB = given

withPostGresDB :: PGS.Connection -> (HasPostGresDB => r) -> r
withPostGresDB = give
