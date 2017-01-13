{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Part of GState DB which stores data necessary for heavyweight delegation.

module Pos.DB.GState.Delegation
       ( getPSKByIssuerAddressHash
       , getPSKByIssuer
       , isIssuerByAddressHash
       , DelegationOp (..)
       , IssuerPublicKey (..)
       , iteratePSKs
       ) where

import           Data.Binary       (Get)
import           Data.Maybe        (isJust)
import qualified Database.RocksDB  as Rocks
import           Universum

import           Pos.Binary.Class  (Bi (..), encodeStrict)
import           Pos.Crypto        (PublicKey, pskDelegatePk, pskIssuerPk)
import           Pos.DB.Class      (MonadDB, getUtxoDB)
import           Pos.DB.DBIterator (DBMapIterator, mapIterator)
import           Pos.DB.Functions  (RocksBatchOp (..), rocksGetBi, WithKeyPrefix (..), encodeWithKeyPrefix)
import           Pos.Types         (AddressHash, ProxySKSimple, StakeholderId,
                                    addressHash, StakeholderId)


----------------------------------------------------------------------------
-- Getters/direct accessors
----------------------------------------------------------------------------

-- | Retrieves certificate by issuer address (hash of public key) if present.
getPSKByIssuerAddressHash :: MonadDB ssc m => StakeholderId -> m (Maybe ProxySKSimple)
getPSKByIssuerAddressHash addrHash =
    rocksGetBi (pskKey $ IssuerPublicKey addrHash) =<< getUtxoDB

-- | Retrieves certificate by issuer public key if present.
getPSKByIssuer :: MonadDB ssc m => PublicKey -> m (Maybe ProxySKSimple)
getPSKByIssuer = getPSKByIssuerAddressHash . addressHash

isIssuerByAddressHash :: MonadDB ssc m => StakeholderId -> m Bool
isIssuerByAddressHash = fmap isJust . getPSKByIssuerAddressHash

----------------------------------------------------------------------------
-- Batch operations
----------------------------------------------------------------------------

data DelegationOp
    = AddPSK !ProxySKSimple
    -- ^ Adds PSK. Overwrites if present.
    | DelPSK !PublicKey
    -- ^ Removes PSK by issuer PK.

instance RocksBatchOp DelegationOp where
    toBatchOp (AddPSK psk)
        | pskIssuerPk psk == pskDelegatePk psk = [] -- panic maybe
        | otherwise =
            [Rocks.Put (pskKey $ IssuerPublicKey $ addressHash $ pskIssuerPk psk)
                       (encodeStrict psk)]
    toBatchOp (DelPSK issuerPk) =
        [Rocks.Del $ pskKey $ IssuerPublicKey $ addressHash issuerPk]

----------------------------------------------------------------------------
-- Iteration
----------------------------------------------------------------------------

type IterType = (IssuerPublicKey,ProxySKSimple)

iteratePSKs :: forall v m ssc a . (MonadDB ssc m, MonadMask m)
                => DBMapIterator IterType v m a -> (IterType -> v) -> m a
iteratePSKs iter f = mapIterator @IterType @v iter f =<< getUtxoDB

----------------------------------------------------------------------------
-- Keys
----------------------------------------------------------------------------

newtype IssuerPublicKey = IssuerPublicKey (AddressHash PublicKey)
    deriving Show

instance Bi IssuerPublicKey where
    put (IssuerPublicKey p) = put p
    get = (get :: Get ByteString) >> IssuerPublicKey <$> get

instance WithKeyPrefix IssuerPublicKey where
    keyPrefix _ = "d/"

-- Storing Hash IssuerPk -> ProxySKSimple
pskKey :: IssuerPublicKey -> ByteString
pskKey = encodeWithKeyPrefix
