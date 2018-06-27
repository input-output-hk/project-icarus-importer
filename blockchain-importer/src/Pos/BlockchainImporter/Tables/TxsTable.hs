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
  , getTxByHash
  , TxRow
  ) where

import           Universum

import qualified Control.Arrow as A
import           Control.Monad (void)
import qualified Data.List.NonEmpty as NE (toList)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import qualified Database.PostgreSQL.Simple as PGS
import           Opaleye
import           Opaleye.RunSelect

import           Pos.BlockchainImporter.Core (TxExtra (..))
import           Pos.BlockchainImporter.Tables.TxAddrTable (TxAddrRowPGR, TxAddrRowPGW,
                                                            transactionAddrTable)
import qualified Pos.BlockchainImporter.Tables.TxAddrTable as TAT (insertTxAddresses)
import           Pos.BlockchainImporter.Tables.Utils
import           Pos.Core (timestampToUTCTimeL)
import           Pos.Core.Txp (Tx (..), TxId, TxOut (..), TxOutAux (..))
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
type TxRow =  ( String, [String], [Int64], [String], [Int64])

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

txAddrTable :: Table TxAddrRowPGW TxAddrRowPGR
txAddrTable = transactionAddrTable "tx_addresses"

-- | Inserts a given Tx into the Tx history tables.
insertTx :: Tx -> TxExtra -> Word64 -> PGS.Connection -> IO ()
insertTx tx txExtra blockHeight conn = do
  insertTxToHistory tx txExtra blockHeight conn
  TAT.insertTxAddresses txAddrTable tx (teInputOutputs txExtra) conn

-- | Inserts the basic info of a given Tx into the master Tx history table.
insertTxToHistory :: Tx -> TxExtra -> Word64 -> PGS.Connection -> IO ()
insertTxToHistory tx txExtra blockHeight conn = void $ runUpsert_ conn txsTable [row]
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
deleteTx :: Tx -> PGS.Connection -> IO ()
deleteTx tx conn = void $ runDelete_  conn $
                                      Delete txsTable (\row -> trHash row .== txHash) rCount
  where
    txHash = pgString $ hashToString (hash tx)

getTxByHash :: TxId -> PGS.Connection -> IO (Maybe TxRow)
getTxByHash txHash conn = do
  txsMatched <- runSelect conn txByHashQuery
  case txsMatched of
    [ txMatched ] -> return $ Just txMatched
    _             -> return Nothing
    where txByHashQuery = proc () -> do
            TxRow hash inputsAddr inputsAmount outputsAddr outputsAmount _ _ <- (selectTable txsTable) -< ()
            restrict -< hash .== (pgString $ hashToString txHash)
            A.returnA -< (hash, inputsAddr, inputsAmount, outputsAddr, outputsAmount)
