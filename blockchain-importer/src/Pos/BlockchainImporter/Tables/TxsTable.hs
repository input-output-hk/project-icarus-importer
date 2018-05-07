{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Pos.BlockchainImporter.Tables.TxsTable
(
  insertTx
) where

import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Time.Clock (UTCTime)
import qualified Database.PostgreSQL.Simple as PGS
import           Opaleye
import           Pos.BlockchainImporter.Core (TxExtra (..))
import           Pos.BlockchainImporter.Tables.Utils (hashToString)
import           Pos.Core (timestampToUTCTimeL)
import           Pos.Core.Txp (Tx (..), TxId)
import           Pos.Crypto (hash)
import           Universum

data TxRowPoly a b c = TxRow {
    trHash     :: a
  , trBlockNum :: b
  , trTime     :: c
  } deriving (Show)

type TxRow = TxRowPoly TxId Word64 UTCTime
type TxRowPGW = TxRowPoly (Column PGText) (Column (Nullable PGInt8)) (Column (Nullable PGTimestamptz))
type TxRowPGR = TxRowPoly (Column PGText) (Column (Nullable PGInt8)) (Column (Nullable PGTimestamptz))

$(makeAdaptorAndInstance "pTxs" ''TxRowPoly)

txsTable :: Table TxRowPGW TxRowPGR
txsTable = Table "txs" (pTxs TxRow { trHash     = required "hash"
                                   , trBlockNum = required "block_num"
                                   , trTime     = required "time"
                                   })

-- FIXME: Replace with inserting a list of txs?
insertTx :: PGS.Connection -> Tx -> TxExtra -> Word64 -> IO ()
insertTx conn tx txExtra blockHeight = do
  putStrLn $ "************* blockHeight: " ++ show blockHeight
  putStrLn $ "************* tx: " ++ show tx
  putStrLn $ "************* txExtra: " ++ show txExtra
  _ <- runInsertMany conn txsTable [row]
  return ()
  where
    row = TxRow {
      trHash     = pgString $ hashToString (hash tx)
    , trBlockNum = toNullable $ pgInt8 $ fromIntegral blockHeight
    , trTime     = maybeToNullable utcTime
    }
    utcTime = pgUTCTime . (^. timestampToUTCTimeL) <$> teReceivedTime txExtra
