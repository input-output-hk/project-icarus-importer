{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Pos.BlockchainImporter.Tables.TxsTable
  ( -- * Data manipulation
    insertTx
  , deleteTx
  ) where

import           Universum

import           Control.Monad (void)
import qualified Data.List.NonEmpty as NE (toList)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import qualified Database.PostgreSQL.Simple as PGS
import           Opaleye

import           Pos.BlockchainImporter.Core (TxExtra (..))
import qualified Pos.BlockchainImporter.Tables.TxAddrTable as TAT (insertTxAddresses)
import           Pos.BlockchainImporter.Tables.Utils
import           Pos.Core (timestampToUTCTimeL)
import           Pos.Core.Txp (Tx (..), TxOut (..), TxOutAux (..))
import           Pos.Crypto (hash)


data TxRowPoly h iAddrs iAmts oAddrs oAmts bn t = TxRow   { trHash          :: h
                                                          , trInputsAddr    :: iAddrs
                                                          , trInputsAmount  :: iAmts
                                                          , trOutputsAddr   :: oAddrs
                                                          , trOutputsAmount :: oAmts
                                                          , trBlockNum      :: bn
                                                          , trTime          :: t
                                                          } deriving (Show)

type TxRowPGW = TxRowPoly (Column PGText)
                          (Column (PGArray PGText))
                          (Column (PGArray PGInt8))
                          (Column (PGArray PGText))
                          (Column (PGArray PGInt8))
                          (Column PGInt8)
                          (Column (Nullable PGTimestamptz))
type TxRowPGR = TxRowPoly (Column PGText)
                          (Column (PGArray PGText))
                          (Column (PGArray PGInt8))
                          (Column (PGArray PGText))
                          (Column (PGArray PGInt8))
                          (Column PGInt8)
                          (Column (Nullable PGTimestamptz))

$(makeAdaptorAndInstance "pTxs" ''TxRowPoly)

txsTable :: Table TxRowPGW TxRowPGR
txsTable = Table "txs" (pTxs TxRow  { trHash            = required "hash"
                                    , trInputsAddr      = required "inputs_address"
                                    , trInputsAmount    = required "inputs_amount"
                                    , trOutputsAddr     = required "outputs_address"
                                    , trOutputsAmount   = required "outputs_amount"
                                    , trBlockNum        = required "block_num"
                                    , trTime            = required "time"
                                    })

-- | Inserts a given Tx into the Tx history tables.
insertTx :: PGS.Connection -> Tx -> TxExtra -> Word64 -> IO ()
insertTx conn tx txExtra blockHeight = PGS.withTransaction conn $ do
  insertTxToHistory conn tx txExtra blockHeight
  TAT.insertTxAddresses conn tx txExtra

-- | Inserts the basic info of a given Tx into the master Tx history table.
insertTxToHistory :: PGS.Connection -> Tx -> TxExtra -> Word64 -> IO ()
insertTxToHistory conn tx txExtra blockHeight = void $ runInsertMany conn txsTable [row]
  where
    inputs  = toaOut <$> (catMaybes $ NE.toList $ teInputOutputs txExtra)
    outputs = NE.toList $ _txOutputs tx
    row = TxRow { trHash          = pgString $ hashToString (hash tx)
                , trInputsAddr    = pgArray (pgString . addressToString . txOutAddress) inputs
                , trInputsAmount  = pgArray (pgInt8 . coinToInt64 . txOutValue) inputs
                , trOutputsAddr   = pgArray (pgString . addressToString . txOutAddress) outputs
                , trOutputsAmount = pgArray (pgInt8 . coinToInt64 . txOutValue) outputs
                , trBlockNum      = pgInt8 $ fromIntegral blockHeight
                  -- FIXME: Tx time should never be None at this stage
                , trTime          = maybeToNullable utcTime
                }
    utcTime = pgUTCTime . (^. timestampToUTCTimeL) <$> teReceivedTime txExtra

-- | Deletes a Tx by Tx hash from the Tx history tables.
deleteTx :: PGS.Connection -> Tx -> IO ()
deleteTx conn tx = void $ runDelete conn txsTable $ \row -> trHash row .== txHash
  where
    txHash = pgString $ hashToString (hash tx)
