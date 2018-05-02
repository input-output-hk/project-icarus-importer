-- | Configuration of cryptographic primitives.

{-# LANGUAGE RankNTypes #-}

module Pos.Crypto.Configuration
       ( ProtocolMagic (..)
       , HasProtocolMagic
       , withProtocolMagic
       , protocolMagic
       , HasPostGresDB
       , withPostGresDB
       , postGresDB
       ) where

import           Universum

import           Data.Reflection (Given (..), give, given)
import qualified Database.PostgreSQL.Simple as PGS

-- | Magic number which should differ for different clusters. It's
-- defined here, because it's used for signing. It also used for other
-- things (e. g. it's part of a serialized block).
newtype ProtocolMagic = ProtocolMagic
    { getProtocolMagic :: Int32
    } deriving (Show, Eq, NFData)

type HasProtocolMagic = Given ProtocolMagic

-- | Protocol magic constant. Is put to block serialized version to
-- distinguish different clusters. It's an example, possible usages
-- are wider.
protocolMagic :: HasProtocolMagic => ProtocolMagic
protocolMagic = given

withProtocolMagic :: ProtocolMagic -> (HasProtocolMagic => r) -> r
withProtocolMagic = give


-- FIXME: MOVE TO THE CORRESPONDANT FILE
type HasPostGresDB = Given PGS.Connection

postGresDB :: HasPostGresDB => PGS.Connection
postGresDB = given

withPostGresDB :: PGS.Connection -> (HasPostGresDB => r) -> r
withPostGresDB = give
