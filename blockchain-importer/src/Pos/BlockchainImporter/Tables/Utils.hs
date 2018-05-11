module Pos.BlockchainImporter.Tables.Utils
  ( hashToString
  , addressToString
  , coinToInt64
  ) where

import           Universum

import           Formatting (sformat)

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
