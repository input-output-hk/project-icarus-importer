{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Pos.BlockchainImporter.Tables.TxsTable where

import           Universum

import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import qualified Database.PostgreSQL.Simple as PGS
import           Opaleye

import           Pos.BlockchainImporter.Tables.Utils
import           Pos.Core.Txp (TxId)

data TxRowPoly a = TxRow {trHash :: a} deriving (Show)

type TxRow = TxRowPoly TxId
type TxRowPGW = TxRowPoly (Column PGText)
type TxRowPGR = TxRowPoly (Column PGText)

$(makeAdaptorAndInstance "pTxs" ''TxRowPoly)

-- FIXME: This currently only stores the tx hash (rename table upon fix)
txsTable :: Table TxRowPGW TxRowPGR
txsTable = Table "temp_txs" (pTxs TxRow {trHash = required "hash"})

-- FIXME: Replace with inserting a list of txs?
-- Inserts a tx into the table
insertTx :: PGS.Connection -> TxId -> IO ()
insertTx conn txHash = do
  _ <- runInsertMany conn txsTable [TxRow (pgString $ hashToString txHash)]
  return ()
