{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Types that arise in the API: mostly simplified representations
-- of the core types which are easier to serialize.
-- Used in purescript-bridge.

module Pos.BlockchainImporter.Web.ClientTypes
       ( CHash (..)
       , CAddress (..)
       , CTxId (..)
       , CEncodedSTx (..)
       , EpochIndex (..)
       , LocalSlotIndex (..)
       , StakeholderId
       , Byte
       , toCHash
       , fromCHash
       , toCAddress
       , fromCAddress
       , toCTxId
       , fromCTxId
       , timestampToPosix
       , encodeHashHex
       , decodeHashHex
       , decodeSTx
       ) where

import           Universum

import           Control.Lens (_Left)
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Buildable
import           Data.Text.Lazy.Builder (fromLazyText)
import qualified Data.Text.Lazy.Encoding as TE
import           Formatting (sformat)
import           Serokell.Data.Memory.Units (Byte)
import           Serokell.Util.Base16 as SB16
import           Servant.API (FromHttpApiData (..))
import           Test.QuickCheck (Arbitrary (..))

import           Pos.Binary (Bi (..))
import qualified Pos.Binary as Bi
import           Pos.Core (Address, EpochIndex, LocalSlotIndex, StakeholderId, addressF,
                           decodeTextAddress, getEpochIndex, getSlotIndex, timestampToPosix)
import           Pos.Core.Txp (TxAux, TxId)
import           Pos.Crypto (AbstractHash, Hash, HashAlgorithm)

import           Pos.BlockchainImporter.TestUtil (secretKeyToAddress)
-------------------------------------------------------------------------------------
-- Hash types
-------------------------------------------------------------------------------------

-- See this page for more explanation - https://cardanodocs.com/cardano/addresses/
-- We have the general type @AbstractHash@ for all hashes we use. It's being parametrized
-- by two types - AbstractHash algo a - the hashing algorithm and the phantom type for
-- extra safety (can be a @Tx@, an @Address@ and so on, ...).
--
-- The following types explain the situation better:
--
-- type AddressHash   = AbstractHash Blake2b_224
-- type Hash          = AbstractHash Blake2b_256
--
-- type TxId          = Hash Tx               = AbstractHash Blake2b_256 Tx
-- type StakeholderId = AddressHash PublicKey = AbstractHash Blake2b_224 PublicKey
--
-- From there on we have the client types that we use to represent the actual hashes.
-- The client types are really the hash bytes converted to Base16 address.

-- | Client hash
newtype CHash = CHash Text
  deriving (Show, Eq, Generic, Buildable, Hashable)

-- | Client address. The address may be from either Cardano or RSCoin.
newtype CAddress = CAddress Text
    deriving (Show, Eq, Generic, Buildable, Hashable)

-- | Client transaction id
newtype CTxId = CTxId CHash
    deriving (Show, Eq, Generic, Buildable, Hashable)

-------------------------------------------------------------------------------------
-- Client-server, server-client transformation functions
-------------------------------------------------------------------------------------

-- | Transformation of core hash-types to client representation.
encodeHashHex
    :: forall algo a. (Bi a)
    => AbstractHash algo a
    -> Text
encodeHashHex = SB16.encode . BA.convert

-- | A required instance for decoding.
instance ToString ByteString where
  toString = toString . SB16.encode

-- | Decoding the text to the original form.
decodeHashHex
    :: forall algo a. (HashAlgorithm algo, Bi (AbstractHash algo a))
    => Text
    -> Either Text (AbstractHash algo a)
decodeHashHex hashText = do
  hashBinary <- SB16.decode hashText
  over _Left toText $ readEither hashBinary

-------------------------------------------------------------------------------------
-- Client hashes functions
-------------------------------------------------------------------------------------

toCHash :: forall a. (Bi a) => Hash a -> CHash
toCHash = CHash . encodeHashHex

fromCHash :: forall a. (Bi (Hash a)) => CHash -> Either Text (Hash a)
fromCHash (CHash h) = decodeHashHex h

toCAddress :: Address -> CAddress
toCAddress = CAddress . sformat addressF

fromCAddress :: CAddress -> Either Text Address
fromCAddress (CAddress addr) = decodeTextAddress addr

toCTxId :: TxId -> CTxId
toCTxId = CTxId . toCHash

fromCTxId :: CTxId -> Either Text TxId
fromCTxId (CTxId (CHash txId)) = decodeHashHex txId

-------------------------------------------------------------------------------------
-- Composite types
-------------------------------------------------------------------------------------

-- | CBOR-encoded signed tx.
newtype CEncodedSTx = CEncodedSTx BSL.ByteString

decodeSTx :: CEncodedSTx -> Either Text TxAux
decodeSTx (CEncodedSTx encodedSTx) = Bi.decodeFull encodedSTx

instance Buildable CEncodedSTx where
    build (CEncodedSTx bs) = fromLazyText $ TE.decodeUtf8 $ B64.encode bs

--------------------------------------------------------------------------------
-- FromHttpApiData instances
--------------------------------------------------------------------------------

instance FromHttpApiData CHash where
    -- Force the free type @a@ to a type `()` so we can get a witness
    -- for the `Bi` and `Typeable` instances.
    parseUrlPiece url = toCHash @() <$> decodeHashHex url

instance FromHttpApiData CAddress where
    parseUrlPiece = pure . CAddress

instance FromHttpApiData CTxId where
    parseUrlPiece = pure . CTxId . CHash

-- TODO: When we have a generic enough `readEither`
-- instance FromHttpApiData LocalSlotIndex where
--     parseUrlPiece = readEither

--------------------------------------------------------------------------------
-- NFData instances
--------------------------------------------------------------------------------

instance NFData CHash

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance Arbitrary CAddress where
    arbitrary = toCAddress . secretKeyToAddress <$> arbitrary
