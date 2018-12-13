{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Pos.BlockchainImporter.Tables.Utils
  ( -- * Conversions
    hashToString
  , addressToString
  , coinToInt64
  , toTxIn
  , toTxOutAux
    -- * Postgres
  , runUpsert_
  ) where

import           Universum

import           Data.List (intercalate)
import           Database.PostgreSQL.Simple ()
import qualified Database.PostgreSQL.Simple as PGS
import           Formatting (sformat)
import qualified Opaleye as O

import           Pos.Core.Common (Address, Coin (..), addressF, decodeTextAddress)
import           Pos.Core.Txp (TxIn (..), TxOut (..), TxOutAux (..))
import           Pos.Crypto (decodeHash, hashHexF)
import           Pos.Crypto.Hashing (AbstractHash)
import           Pos.Txp.Toil.Types ()

----------------------------------------------------------------------------
-- Conversions
----------------------------------------------------------------------------

hashToString :: AbstractHash algo a -> String
hashToString h = toString $ sformat hashHexF h

cutDownLongAddress :: String -> String
cutDownLongAddress = take 7000

addressToString :: Address -> String
addressToString addr = cutDownLongAddress . toString $ sformat addressF addr

coinToInt64 :: Coin -> Int64
coinToInt64 = fromIntegral . getCoin

toTxOutAux :: Text -> Int64 -> Maybe TxOutAux
toTxOutAux receiver amount = do
  decReceiver <- rightToMaybe $ decodeTextAddress receiver
  pure $ TxOutAux $ TxOut decReceiver (Coin $ fromIntegral amount)

toTxIn :: Text -> Int -> Maybe TxIn
toTxIn txHash idx = do
  decTxHash <- rightToMaybe $ decodeHash txHash
  pure $ TxInUtxo decTxHash (fromIntegral idx)

----------------------------------------------------------------------------
-- Postgres
----------------------------------------------------------------------------

{-
    Upserts rows into a table:
    - If the rows are not present (checked by the primary keys passed), they are inserted.
    - If they are, the columns specified are updated, none is updated if this list is empty.

    FIXME: Due to ON CONFLICT (..) DO UPDATE is not yet implemented by Opaleye this had to
    be manually implemented, which involved using the deprecated function
    'arrangeInsertManySql'. Once this feature gets released, the usage of this
    function will be removed.
-}
runUpsert_ ::
     PGS.Connection             -- Connection to the SQL DB
  -> O.Table columns columns'   -- Table to perform the upsert on
  -> [String]                   -- Primary keys to check if columns are present
  -> [String]                   -- Names of the columns to update
  -> [columns]                  -- Rows to insert
  -> IO Int64
runUpsert_ conn table pkCheckConflict colUpdateOnConflict columns = case nonEmpty columns of
    Just neColumns -> PGS.execute_ conn . fromString $ strUpsertQuery neColumns
    Nothing        -> return 0
  where strInsertQuery col = O.arrangeInsertManySql table col
        strUpsertQuery col = (strInsertQuery col)  ++ " ON CONFLICT (" ++ pksString  ++ ") " ++
                             updateColumnsToOnConflict colUpdateOnConflict
        pksString      = intercalate ", " pkCheckConflict


----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

updateColumnsToOnConflict :: [String] -> String
updateColumnsToOnConflict []              = "DO NOTHING"
updateColumnsToOnConflict columnsToUpdate = "DO UPDATE SET " ++ doUpdateSet
  where doUpdateSet = intercalate ", " $ (\col -> col ++ "=EXCLUDED." ++ col) <$> columnsToUpdate
