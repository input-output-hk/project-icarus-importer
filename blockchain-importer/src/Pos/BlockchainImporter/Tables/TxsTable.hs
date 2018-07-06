{-# LANGUAGE Arrows #-}

module Pos.BlockchainImporter.Tables.TxsTable
  ( -- * Manipulation
    insertConfirmedTx
  , insertFailedTx
  , deleteTx
  ) where

import           Universum

import           Control.Monad (void)
import qualified Data.List.NonEmpty as NE (toList)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import qualified Database.PostgreSQL.Simple as PGS
import           Opaleye

import           Pos.BlockchainImporter.Core (TxExtra (..))
import           Pos.BlockchainImporter.Tables.TxAddrTable (TxAddrRowPGR, TxAddrRowPGW,
                                                            transactionAddrTable)
import qualified Pos.BlockchainImporter.Tables.TxAddrTable as TAT (insertTxAddresses)
import           Pos.BlockchainImporter.Tables.Utils
import           Pos.Core (timestampToUTCTimeL)
import           Pos.Core.Txp (Tx (..), TxId, TxOut (..), TxOutAux (..))
import           Pos.Crypto (hash)

data TxRowPoly h iAddrs iAmts oAddrs oAmts bn t c = TxRow   { trHash          :: h
                                                            , trInputsAddr    :: iAddrs
                                                            , trInputsAmount  :: iAmts
                                                            , trOutputsAddr   :: oAddrs
                                                            , trOutputsAmount :: oAmts
                                                            , trBlockNum      :: bn
                                                            , trTime          :: t
                                                            , trSucceeded     :: c
                                                            } deriving (Show)

type TxRowPGW = TxRowPoly (Column PGText)
                          (Column (PGArray PGText))
                          (Column (PGArray PGInt8))
                          (Column (PGArray PGText))
                          (Column (PGArray PGInt8))
                          (Column (Nullable PGInt8))
                          (Column (Nullable PGTimestamptz))
                          (Column PGBool)
type TxRowPGR = TxRowPoly (Column PGText)
                          (Column (PGArray PGText))
                          (Column (PGArray PGInt8))
                          (Column (PGArray PGText))
                          (Column (PGArray PGInt8))
                          (Column (Nullable PGInt8))
                          (Column (Nullable PGTimestamptz))
                          (Column PGBool)

$(makeAdaptorAndInstance "pTxs" ''TxRowPoly)

txsTable :: Table TxRowPGW TxRowPGR
txsTable = Table "txs" (pTxs TxRow  { trHash            = required "hash"
                                    , trInputsAddr      = required "inputs_address"
                                    , trInputsAmount    = required "inputs_amount"
                                    , trOutputsAddr     = required "outputs_address"
                                    , trOutputsAmount   = required "outputs_amount"
                                    , trBlockNum        = required "block_num"
                                    , trTime            = required "time"
                                    , trSucceeded       = required "succeeded"
                                    })

txAddrTable :: Table TxAddrRowPGW TxAddrRowPGR
txAddrTable = transactionAddrTable "tx_addresses"

insertConfirmedTx :: Tx -> TxExtra -> Word64 -> PGS.Connection -> IO ()
insertConfirmedTx tx txExtra blockHeight conn = insertTx tx txExtra (Just blockHeight) True conn

insertFailedTx :: Tx -> TxExtra -> PGS.Connection -> IO ()
insertFailedTx tx txExtra conn = insertTx tx txExtra Nothing False conn

-- | Inserts a given Tx into the Tx history tables.
insertTx :: Tx -> TxExtra -> Maybe Word64 -> Bool -> PGS.Connection -> IO ()
insertTx tx txExtra maybeBlockHeight succeeded conn = do
  insertTxToHistory tx txExtra maybeBlockHeight succeeded conn
  TAT.insertTxAddresses txAddrTable tx (teInputOutputs txExtra) conn

-- | Inserts the basic info of a given Tx into the master Tx history table.
insertTxToHistory :: Tx -> TxExtra -> Maybe Word64 -> Bool -> PGS.Connection -> IO ()
insertTxToHistory tx TxExtra{..} blockHeight succeeded conn = void $ runUpsert_ conn txsTable [row]
  where
    inputs  = toaOut <$> (catMaybes $ NE.toList $ teInputOutputs)
    outputs = NE.toList $ _txOutputs tx
    row = TxRow { trHash          = pgString $ hashToString (hash tx)
                , trInputsAddr    = pgArray (pgString . addressToString . txOutAddress) inputs
                , trInputsAmount  = pgArray (pgInt8 . coinToInt64 . txOutValue) inputs
                , trOutputsAddr   = pgArray (pgString . addressToString . txOutAddress) outputs
                , trOutputsAmount = pgArray (pgInt8 . coinToInt64 . txOutValue) outputs
                , trBlockNum      = fromMaybe (Opaleye.null) $
                                              (toNullable . pgInt8 . fromIntegral) <$> blockHeight
                  -- FIXME: Tx time should never be None at this stage
                , trTime          = maybeToNullable utcTime
                , trSucceeded     = pgBool succeeded
                }
    utcTime = pgUTCTime . (^. timestampToUTCTimeL) <$> teFullProcessTime

-- | Deletes a Tx by Tx hash from the Tx history tables.
deleteTx :: TxId -> PGS.Connection -> IO ()
deleteTx txId conn = void $ runDelete_  conn $
                                      Delete txsTable (\row -> trHash row .== txHash) rCount
  where
    txHash = pgString $ hashToString txId
