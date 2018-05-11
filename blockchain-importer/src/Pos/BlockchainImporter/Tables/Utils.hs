{-# LANGUAGE OverloadedStrings #-}

module Pos.BlockchainImporter.Tables.Utils
  ( hashToString
  , addressToString
  , coinToInt64
  ) where

import           Universum

import qualified Data.List.NonEmpty as NE
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

{-  Insert rows into a table, only if they are not already present

    FIXME: Due to upsert not being yet implemented by Opaleye [1] this had to
    be manually implemented, which involved using the deprecated function
    'arrangeInsertManySql'. Once this feature gets released, the usage of this
    function will be removed.

    [1] https://github.com/tomjaguarpaw/haskell-opaleye/pull/385#issuecomment-384313025
-}
runUpsertMany :: PGS.Connection	-> O.Table columns columns'	-> [columns] -> IO Int64
runUpsertMany conn table columns = case nonEmpty columns of
    Just neColumns -> PGS.execute_ conn $ ((strUpsertQuery neColumns) :: PGS.Query)
    Nothing        -> return 0
  where strInsertQuery col = (O.arrangeInsertManySql table col) :: String
        strUpsertQuery col = (strInsertQuery col)  ++ " ON CONFLICT NOTHING"

