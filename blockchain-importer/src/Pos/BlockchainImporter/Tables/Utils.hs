{-# LANGUAGE OverloadedStrings #-}

module Pos.BlockchainImporter.Tables.Utils
  ( -- * Conversions
    hashToString
  , addressToString
  , coinToInt64
  , toTxOutAux
    -- * Postgres
  , runUpsert_
  ) where

import           Universum

import           Database.PostgreSQL.Simple ()
import qualified Database.PostgreSQL.Simple as PGS
import           Formatting (sformat)
import qualified Opaleye as O

import           Pos.Core.Common (Address, Coin (..), addressF, decodeTextAddress)
import           Pos.Core.Txp (TxOut (..), TxOutAux (..))
import           Pos.Crypto (hashHexF)
import           Pos.Crypto.Hashing (AbstractHash)
import           Pos.Txp.Toil.Types ()

----------------------------------------------------------------------------
-- Conversions
----------------------------------------------------------------------------

hashToString :: AbstractHash algo a -> String
hashToString h = toString $ sformat hashHexF h

addressToString :: Address -> String
addressToString addr = toString $ sformat addressF addr

coinToInt64 :: Coin -> Int64
coinToInt64 = fromIntegral . getCoin

toTxOutAux :: Text -> Int64 -> Maybe TxOutAux
toTxOutAux receiver amount = do
  decReceiver <- rightToMaybe $ decodeTextAddress receiver
  pure $ TxOutAux $ TxOut decReceiver (Coin $ fromIntegral amount)

----------------------------------------------------------------------------
-- Postgres
----------------------------------------------------------------------------

-- Insert rows into a table, only if they are not already present
runUpsert_ :: PGS.Connection -> O.Table columns columns' -> [columns] -> IO Int64
runUpsert_ conn table columns = O.runInsert_ conn sqlInsert
  where sqlInsert = O.Insert table columns O.rCount (Just O.DoNothing)
