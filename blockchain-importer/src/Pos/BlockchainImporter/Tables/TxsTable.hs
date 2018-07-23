{-# LANGUAGE Arrows #-}

module Pos.BlockchainImporter.Tables.TxsTable
  ( -- * Data types
    TxRecord (..)
    -- * Getters
  , getTxByHash
    -- * Manipulation
  , upsertSuccessfulTx
  , upsertFailedTx
  , upsertPendingTx
  , markPendingTxsAsFailed
  ) where

import           Universum

import qualified Control.Arrow as A
import           Control.Lens (from)
import           Control.Monad (void)
import qualified Data.List.NonEmpty as NE (toList)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Time.Clock (UTCTime, getCurrentTime)
import qualified Database.PostgreSQL.Simple as PGS
import           Opaleye
import           Opaleye.RunSelect

import           Pos.BlockchainImporter.Core (TxExtra (..))
import qualified Pos.BlockchainImporter.Tables.TxAddrTable as TAT (insertTxAddresses)
import           Pos.BlockchainImporter.Tables.Utils
import           Pos.Core (BlockCount, Timestamp, timestampToUTCTimeL)
import           Pos.Core.Txp (Tx (..), TxId, TxOut (..), TxOutAux (..), TxUndo)
import           Pos.Crypto (hash)

-- txTimestamp corresponds to the trTimestamp
data TxRecord = TxRecord
    { txHash      :: !TxId
    , txInputs    :: !(NonEmpty TxOutAux)
    , txOutputs   :: !(NonEmpty TxOutAux)
    , txBlockNum  :: !(Maybe Int64)
    , txTimestamp :: !(Maybe Timestamp)
    , txState     :: !TxState
    }

{-|
    Tx state machine:
                          tx included
                            in block
                       --------------->
                      |                 Succesful
                      |   -------------     ^
                      |  |    block         |
                      |  |  rollbacked      |
        tx created    |  V                  |
      -------------> Pending                | Chain reorganization
                      ^  |                  |
                      |  | tx becomes       |
                      |  |   invalid        |
                      |   ------------->    |
                      |                  Failed
                       -----------------
                           tx resend
-}
data TxState  = Successful
              | Failed
              | Pending
              deriving (Show, Read)

{-|
    Given the possible events:
    - Tx gets confirmed (tx becomes Successful)
    - Tx sending fails (tx becomes Failed)
    - Tx gets created (tx becomes Pending)
    The difference between the trTimestamp and trLastUpdate is that:
    - trTimestamp is the time the event happened, that the tx changed it's state
    - trLastUpdate is the time the importer learned of this event

    The main case where both timestamp differ is when syncing from peers, trTimestamp is
    the moment that block was created (no matter how long ago this was) while trLastUpdate
    is the moment the importer received the block.

    Both are needed, as trTimestap is the one the user is interested of knowing, while
    trLastUpdate is used for fetching those events
-}
data TxRowPoly h iAddrs iAmts oAddrs oAmts bn t state last = TxRow  { trHash          :: h
                                                                    , trInputsAddr    :: iAddrs
                                                                    , trInputsAmount  :: iAmts
                                                                    , trOutputsAddr   :: oAddrs
                                                                    , trOutputsAmount :: oAmts
                                                                    , trBlockNum      :: bn
                                                                    , trTimestamp     :: t
                                                                    , trState         :: state
                                                                    , trLastUpdate    :: last
                                                                    } deriving (Show)

type TxRowPG = TxRowPoly  (Column PGText)                   -- Tx hash
                          (Column (PGArray PGText))         -- Inputs addresses
                          (Column (PGArray PGInt8))         -- Inputs amounts
                          (Column (PGArray PGText))         -- Outputs addresses
                          (Column (PGArray PGInt8))         -- Outputs amounts
                          (Column (Nullable PGInt8))        -- Block number
                          (Column (Nullable PGTimestamptz)) -- Timestamp tx moved to current state
                          (Column PGText)                   -- Tx state
                          (Column PGTimestamptz)            -- Timestamp of the last update

$(makeAdaptorAndInstance "pTxs" ''TxRowPoly)

txsTable :: Table TxRowPG TxRowPG
txsTable = Table "txs" (pTxs TxRow  { trHash            = required "hash"
                                    , trInputsAddr      = required "inputs_address"
                                    , trInputsAmount    = required "inputs_amount"
                                    , trOutputsAddr     = required "outputs_address"
                                    , trOutputsAmount   = required "outputs_amount"
                                    , trBlockNum        = required "block_num"
                                    , trTimestamp       = required "time"
                                    , trState           = required "tx_state"
                                    , trLastUpdate      = required "last_update"
                                    })


----------------------------------------------------------------------------
-- Getters and manipulation
----------------------------------------------------------------------------

-- | Returns a tx by hash
getTxByHash :: TxId -> PGS.Connection -> IO (Maybe TxRecord)
getTxByHash txHash conn = do
  txsMatched  :: [(Text, [Text], [Int64], [Text], [Int64], Maybe Int64, Maybe UTCTime, String)]
              <- runSelect conn txByHashQuery
  pure $ case txsMatched of
    [ ((_, inpAddrs, inpAmounts, outAddrs, outAmounts, blkNum, t, txStateString)) ] -> do
      inputs <- zipWithM toTxOutAux inpAddrs inpAmounts >>= nonEmpty
      outputs <- zipWithM toTxOutAux outAddrs outAmounts >>= nonEmpty
      txState <- readMaybe txStateString
      let time = t <&> (^. from timestampToUTCTimeL)
      pure $ TxRecord txHash inputs outputs blkNum time txState
    _ -> Nothing
    where txByHashQuery = proc () -> do
            TxRow rowTxHash inputsAddr inputsAmount outputsAddr outputsAmount blkNum t txState _ <- (selectTable txsTable) -< ()
            restrict -< rowTxHash .== (pgString $ hashToString txHash)
            A.returnA -< (rowTxHash, inputsAddr, inputsAmount, outputsAddr, outputsAmount, blkNum, t, txState)

{-|
    Inserts a confirmed tx to the tx history table
    If the tx was already present with a different state, it is moved to the confirmed one and
    it's timestamp and last update are updated
-}
upsertSuccessfulTx :: Tx -> TxExtra -> BlockCount -> PGS.Connection -> IO ()
upsertSuccessfulTx tx txExtra blockHeight conn = upsertTx tx txExtra (Just blockHeight) Successful conn

{-|
    Inserts a failed tx to the tx history table with the current time as it's timestamp
    If the tx was already present with a different state, it is moved to the failed one and
    it's timestamp and last update are updated
-}
upsertFailedTx :: Tx -> TxUndo -> PGS.Connection -> IO ()
upsertFailedTx tx txUndo conn = do
  txExtra <- currentTxExtra txUndo
  upsertTx tx txExtra Nothing Failed conn

{-|
    Inserts a pending tx to the tx history table with the current time as it's timestamp
    If the tx was already present with a different state, it is moved to the pending one and
    it's timestamp and last update are updated
-}
upsertPendingTx :: Tx -> TxUndo -> PGS.Connection -> IO ()
upsertPendingTx tx txUndo conn = do
  txExtra <- currentTxExtra txUndo
  upsertTx tx txExtra Nothing Pending conn

{-|
    Marks all pending txs as failed
    All the txs changed have their timestamp and last update changed to the current time
-}
markPendingTxsAsFailed :: PGS.Connection -> IO ()
markPendingTxsAsFailed conn = do
  currentTime <- getCurrentTime
  let changePendingToFailed row = row
                                    { trState      = show Failed
                                    , trTimestamp  = toNullable $ pgUTCTime currentTime
                                    , trLastUpdate = pgUTCTime currentTime
                                    }
      isPending row             = trState row .== show Pending
  void $ runUpdate_ conn $ Update txsTable changePendingToFailed isPending rCount


----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- Inserts a given Tx into the Tx history tables with a given state (overriding any
-- it if it was already present).
upsertTx :: Tx -> TxExtra -> Maybe BlockCount -> TxState -> PGS.Connection -> IO ()
upsertTx tx txExtra maybeBlockHeight succeeded conn = do
  upsertTxToHistory tx txExtra maybeBlockHeight succeeded conn
  TAT.insertTxAddresses tx (teInputOutputs txExtra) conn

-- Inserts the basic info of a given Tx into the master Tx history table (overriding any
-- it if it was already present)
upsertTxToHistory :: Tx -> TxExtra -> Maybe BlockCount -> TxState -> PGS.Connection -> IO ()
upsertTxToHistory tx TxExtra{..} blockHeight txState conn = do
  currentTime <- getCurrentTime
  void $ runUpsert_ conn txsTable ["hash"]
                    ["block_num", "tx_state", "last_update", "time"]
                    [rowFromLastUpdate currentTime]
  where
    inputs                        = toaOut <$> (catMaybes $ NE.toList $ teInputOutputs)
    outputs                       = NE.toList $ _txOutputs tx
    rowFromLastUpdate currentTime =
          TxRow { trHash          = pgString $ hashToString (hash tx)
                , trInputsAddr    = pgArray (pgString . addressToString . txOutAddress) inputs
                , trInputsAmount  = pgArray (pgInt8 . coinToInt64 . txOutValue) inputs
                , trOutputsAddr   = pgArray (pgString . addressToString . txOutAddress) outputs
                , trOutputsAmount = pgArray (pgInt8 . coinToInt64 . txOutValue) outputs
                , trBlockNum      = fromMaybe (Opaleye.null) $
                                              (toNullable . pgInt8 . fromIntegral) <$> blockHeight
                  -- FIXME: Tx time should never be None at this stage
                , trTimestamp     = maybeToNullable $ timestampToPGTime <$> teTimestamp
                , trState         = pgString $ show txState
                , trLastUpdate    = pgUTCTime currentTime
                }
    timestampToPGTime = pgUTCTime . (^. timestampToUTCTimeL)

currentTxExtra :: TxUndo -> IO TxExtra
currentTxExtra txUndo = do
  currentTime <- getCurrentTime
  let currentTimestamp = currentTime ^. from timestampToUTCTimeL
  pure $ TxExtra (Just currentTimestamp) txUndo
