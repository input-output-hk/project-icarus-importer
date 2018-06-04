module Pos.BlockchainImporter.Tables.PendingTxsTable
  ( -- * Data manipulation
    insertPendingTx
  , deletePendingTx
  , clearPendingTx
  ) where

import           Universum

import qualified Data.List.NonEmpty as NE (toList)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Time.Clock.POSIX (getCurrentTime)
import qualified Database.PostgreSQL.Simple as PGS
import           Opaleye

import           Pos.BlockchainImporter.Tables.TxAddrTable (TxAddrRowPGR, TxAddrRowPGW,
                                                            addressToTxTable)
import qualified Pos.BlockchainImporter.Tables.TxAddrTable as TAT (insertTxAddresses)
import           Pos.BlockchainImporter.Tables.Utils
import           Pos.Core.Txp (Tx (..), TxOut (..), TxOutAux (..), TxUndo)
import           Pos.Crypto (hash)

data PTxRowPoly h iAddrs iAmts oAddrs oAmts t = PTxRow  { ptrHash          :: h
                                                        , ptrInputsAddr    :: iAddrs
                                                        , ptrInputsAmount  :: iAmts
                                                        , ptrOutputsAddr   :: oAddrs
                                                        , ptrOutputsAmount :: oAmts
                                                        , ptrCreatedTime   :: t
                                                        } deriving (Show)

type PTxRowPGW = PTxRowPoly (Column PGText)
                            (Column (PGArray PGText))
                            (Column (PGArray PGInt8))
                            (Column (PGArray PGText))
                            (Column (PGArray PGInt8))
                            (Column (PGTimestamptz))
type PTxRowPGR = PTxRowPoly (Column PGText)
                            (Column (PGArray PGText))
                            (Column (PGArray PGInt8))
                            (Column (PGArray PGText))
                            (Column (PGArray PGInt8))
                            (Column (PGTimestamptz))

$(makeAdaptorAndInstance "pPTxs" ''PTxRowPoly)

pTxsTable :: Table PTxRowPGW PTxRowPGR
pTxsTable = Table "pending_txs" (pPTxs PTxRow { ptrHash           = required "hash"
                                              , ptrInputsAddr     = required "inputs_address"
                                              , ptrInputsAmount   = required "inputs_amount"
                                              , ptrOutputsAddr    = required "outputs_address"
                                              , ptrOutputsAmount  = required "outputs_amount"
                                              , ptrCreatedTime    = required "created_time"
                                              })

ptxAddrTable :: Table TxAddrRowPGW TxAddrRowPGR
ptxAddrTable = addressToTxTable "ptx_addresses"

-- | Inserts a given pending Tx into the pending tx tables.
insertPendingTx :: Tx -> TxUndo -> PGS.Connection -> IO ()
insertPendingTx tx txUndo conn = do
  insertPTxToHistory conn tx txUndo
  TAT.insertTxAddresses ptxAddrTable conn tx txUndo

-- | Inserts the info of a given pending Tx into the master pending Tx table.
insertPTxToHistory :: PGS.Connection -> Tx -> TxUndo -> IO ()
insertPTxToHistory conn tx txUndo = do
  insertionTime <- getCurrentTime
  void $ runUpsert_ conn pTxsTable [rowFromTime insertionTime]
    where
      inputs  = toaOut <$> (catMaybes $ NE.toList $ txUndo)
      outputs = NE.toList $ _txOutputs tx
      rowFromTime currTime =
        PTxRow  { ptrHash          = pgString $ hashToString (hash tx)
                , ptrInputsAddr    = pgArray (pgString . addressToString . txOutAddress) inputs
                , ptrInputsAmount  = pgArray (pgInt8 . coinToInt64 . txOutValue) inputs
                , ptrOutputsAddr   = pgArray (pgString . addressToString . txOutAddress) outputs
                , ptrOutputsAmount = pgArray (pgInt8 . coinToInt64 . txOutValue) outputs
                , ptrCreatedTime   = pgUTCTime currTime
                }

-- | Deletes a pending Tx by Tx hash from the pending tx tables.
deletePendingTx :: Tx -> PGS.Connection -> IO ()
deletePendingTx tx conn = void $ runDelete_   conn $
                                              Delete pTxsTable (\row -> ptrHash row .== txHash) rCount
  where txHash = pgString $ hashToString (hash tx)

-- | Deletes all pending tx from the pending tx tables
clearPendingTx :: PGS.Connection -> IO ()
clearPendingTx conn = void $ runDelete_ conn $ Delete pTxsTable (const $ pgBool True) rCount
