{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Pos.BlockchainImporter.Tables.TxAddrTable
  ( -- * Data manipulation
    insertTxAddresses
  ) where

import           Universum

import           Control.Monad (void)
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE (toList)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import qualified Database.PostgreSQL.Simple as PGS
import           Opaleye

import           Pos.BlockchainImporter.Core (TxExtra (..))
import           Pos.BlockchainImporter.Tables.Utils
import           Pos.Core.Common (Address)
import           Pos.Core.Txp (Tx (..), TxOut (..), TxOutAux (..))
import           Pos.Crypto (hash)

data TxAddrRowPoly a b = TxAddrRow  { taHash    :: a
                                    , taAddress :: b
                                    } deriving (Show)

type TxAddrRowPGW = TxAddrRowPoly (Column PGText) (Column PGText)
type TxAddrRowPGR = TxAddrRowPoly (Column PGText) (Column PGText)

$(makeAdaptorAndInstance "pTxAddr" ''TxAddrRowPoly)

txAddressesTable :: Table TxAddrRowPGW TxAddrRowPGR
txAddressesTable = Table "tx_addresses" (pTxAddr TxAddrRow  { taHash    = required "tx_hash"
                                                            , taAddress = required "address"
                                                            })

-- | Creates a row for the Tx addresses from a given Tx input/output.
makeRowPGW :: String -> Address -> TxAddrRowPGW
makeRowPGW txHash txAddr = TxAddrRow {..}
  where
    taHash    = pgString txHash
    taAddress = pgString address
    address   = addressToString $ txAddr

-- | Inserts the senders and receivers of a given Tx into the Tx addresses table.
insertTxAddresses :: PGS.Connection -> Tx -> TxExtra -> IO ()
insertTxAddresses conn tx txExtra = void $ runUpsert_ conn txAddressesTable rows
  where
    txHash    = hashToString (hash tx)
    senders   = txOutAddress . toaOut <$> (catMaybes $ NE.toList $ teInputOutputs txExtra)
    receivers = txOutAddress <$> (NE.toList $ _txOutputs tx)
    rows      = makeRows $ L.nub $ senders ++ receivers
    makeRows  = map (makeRowPGW txHash)
