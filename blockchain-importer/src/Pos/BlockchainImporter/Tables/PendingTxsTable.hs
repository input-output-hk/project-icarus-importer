{-# LANGUAGE Arrows #-}
module Pos.BlockchainImporter.Tables.PendingTxsTable
  ( PTxRecord (..)
    -- * Getters
  , getPendingTxByHash
    -- * Manipulation
  , insertPendingTx
  , deletePendingTx
  , clearPendingTx
  ) where

import           Universum

import qualified Control.Arrow as A
import           Control.Lens (from)
import qualified Data.List.NonEmpty as NE (toList)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Time.Clock (UTCTime)
import           Data.Time.Clock.POSIX (getCurrentTime)
import qualified Database.PostgreSQL.Simple as PGS
import           Opaleye
import           Opaleye.RunSelect

import           Pos.BlockchainImporter.Tables.TxAddrTable (TxAddrRowPGR, TxAddrRowPGW,
                                                            transactionAddrTable)
import qualified Pos.BlockchainImporter.Tables.TxAddrTable as TAT (insertTxAddresses)
import           Pos.BlockchainImporter.Tables.Utils
import           Pos.Core (Timestamp, timestampToUTCTimeL)
import           Pos.Core.Txp (Tx (..), TxId, TxOut (..), TxOutAux (..), TxUndo)
import           Pos.Crypto (hash)

data PTxRecord = PTxRecord
    { ptxHash        :: !TxId
    , ptxInputs      :: !(NonEmpty TxOutAux)
    , ptxOutputs     :: !(NonEmpty TxOutAux)
    , ptxCreatedTime :: !Timestamp
    }

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

pendingTxsTable :: Table PTxRowPGW PTxRowPGR
pendingTxsTable = Table "pending_txs" (pPTxs PTxRow { ptrHash           = required "hash"
                                                    , ptrInputsAddr     = required "inputs_address"
                                                    , ptrInputsAmount   = required "inputs_amount"
                                                    , ptrOutputsAddr    = required "outputs_address"
                                                    , ptrOutputsAmount  = required "outputs_amount"
                                                    , ptrCreatedTime    = required "created_time"
                                                    })

pendingTxAddrTable :: Table TxAddrRowPGW TxAddrRowPGR
pendingTxAddrTable = transactionAddrTable "ptx_addresses"

-- | Inserts a given pending Tx into the pending tx tables.
insertPendingTx :: Tx -> TxUndo -> PGS.Connection -> IO ()
insertPendingTx tx txUndo conn = do
  insertPendingTxToHistory tx txUndo conn
  TAT.insertTxAddresses pendingTxAddrTable tx txUndo conn

-- | Inserts the info of a given pending Tx into the master pending Tx table.
insertPendingTxToHistory :: Tx -> TxUndo -> PGS.Connection -> IO ()
insertPendingTxToHistory tx txUndo conn = do
  insertionTime <- getCurrentTime
  void $ runUpsert_ conn pendingTxsTable [rowFromTime insertionTime]
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
deletePendingTx :: TxId -> PGS.Connection -> IO ()
deletePendingTx txHash conn = void $ runDelete_   conn $
                                              Delete pendingTxsTable (\row -> ptrHash row .== pgTxHash) rCount
  where pgTxHash = pgString $ hashToString txHash

-- | Get pending tx by hash
getPendingTxByHash :: TxId -> PGS.Connection -> IO (Maybe PTxRecord)
getPendingTxByHash txHash conn = do
  txsMatched :: [(Text, [Text], [Int64], [Text], [Int64], UTCTime)] <- runSelect conn ptxByHashQuery
  case txsMatched of
    [ (_, inpAddrs, inpAmounts, outAddrs, outAmounts, t) ] -> pure $ do
      inputs <- zipWithM toTxOutAux inpAddrs inpAmounts >>= nonEmpty
      outputs <- zipWithM toTxOutAux outAddrs outAmounts >>= nonEmpty
      let time = t ^. from timestampToUTCTimeL
      pure $ PTxRecord txHash inputs outputs time
    _ ->
      pure Nothing
    where ptxByHashQuery = proc () -> do
            PTxRow rowTxHash inpAddr inpAmount outAddr outAmount createdTime <- (selectTable pendingTxsTable) -< ()
            restrict -< rowTxHash .== (pgString $ hashToString txHash)
            A.returnA -< (rowTxHash, inpAddr, inpAmount, outAddr, outAmount, createdTime)

-- | Deletes all pending tx from the pending tx tables
clearPendingTx :: PGS.Connection -> IO ()
clearPendingTx conn = void $ runDelete_ conn $ Delete pendingTxsTable (const $ pgBool True) rCount
