{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Pos.BlockchainImporter.Txp.Toil.TxsTable where

import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import qualified Database.PostgreSQL.Simple as PGS
import           Opaleye

import           Universum

type TxHash = String

data TxRowPoly a = TxRow {trHash :: a} deriving (Show)

type TxRow = TxRowPoly TxHash
type TxRowPGW = TxRowPoly (Column PGText)
type TxRowPGR = TxRowPoly (Column PGText)

$(makeAdaptorAndInstance "pTxs" ''TxRowPoly)

-- FIXME: This currently only stores the tx hash (rename table upon fix)
txsTable :: Table TxRowPGW TxRowPGR
txsTable = Table "temp_txs" (pTxs TxRow {trHash = required "hash"})

-- FIXME: Replace with inserting a list of txs?
-- Inserts a tx into the table
insertTx :: PGS.Connection -> TxHash -> IO ()
insertTx conn txHash = do
  rows <- runInsertMany conn txsTable [TxRow (pgString txHash)]
  putStrLn $ show rows ++ " row(s) inserted"  -- FIXME: Delete, for debugging purposes only

-- FIXME: Delete, not needed
-- Returns all the txs stored in the table
getAllTxs :: PGS.Connection -> IO ()
getAllTxs conn = do
  txs <- runQuery conn (queryTable txsTable) :: IO [TxRow]
  print txs

