{-# LANGUAGE OverloadedStrings #-}

module Pos.BlockchainImporter.Tables.Utils
  ( hashToString
  , addressToString
  , coinToInt64
  , runUpsertMany
  ) where

import           Universum

import           Database.PostgreSQL.Simple ()
import qualified Database.PostgreSQL.Simple as PGS
import           Formatting (sformat)
import qualified Opaleye as O

import           Pos.Core.Common (Address, Coin (..), addressF)
import           Pos.Crypto (hashHexF)
import           Pos.Crypto.Hashing (AbstractHash)
import           Pos.Txp.Toil.Types ()

hashToString :: AbstractHash algo a -> String
hashToString h = toString $ sformat hashHexF h

addressToString :: Address -> String
addressToString addr = toString $ sformat addressF addr

coinToInt64 :: Coin -> Int64
coinToInt64 = fromIntegral . getCoin

-- Insert rows into a table, only if they are not already present
runUpsertMany :: PGS.Connection -> O.Table columns columns' -> [columns] -> IO Int64
runUpsertMany conn table columns = O.runInsert_ conn sqlInsert
  where sqlInsert = O.Insert table columns O.rCount (Just O.DoNothing)
