{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Types that arise in the API: mostly simplified representations
-- of the core types which are easier to serialize.
-- Used in purescript-bridge.

module Pos.BlockchainImporter.Web.ClientTypes
       ( CEncodedSTx (..)
       , EpochIndex (..)
       , LocalSlotIndex (..)
       , StakeholderId
       , Byte
       , timestampToPosix
       , decodeSTx
       ) where

import           Universum

import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Buildable
import           Data.Text.Lazy.Builder (fromLazyText)
import qualified Data.Text.Lazy.Encoding as TE
import           Serokell.Data.Memory.Units (Byte)

import qualified Pos.Binary as Bi
import           Pos.Core (EpochIndex, LocalSlotIndex, StakeholderId, getEpochIndex, getSlotIndex,
                           timestampToPosix)
import           Pos.Core.Txp (TxAux)

-------------------------------------------------------------------------------------
-- Composite types
-------------------------------------------------------------------------------------

-- | CBOR-encoded signed tx.
newtype CEncodedSTx = CEncodedSTx BSL.ByteString deriving (Generic)

decodeSTx :: CEncodedSTx -> Either Text TxAux
decodeSTx (CEncodedSTx encodedSTx) = Bi.decodeFull encodedSTx

instance Buildable CEncodedSTx where
    build (CEncodedSTx bs) = fromLazyText $ TE.decodeUtf8 $ B64.encode bs

