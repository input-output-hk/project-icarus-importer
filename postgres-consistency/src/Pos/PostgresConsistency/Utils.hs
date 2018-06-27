module Pos.PostgresConsistency.Utils where

import           Universum

import           Pos.Core (Address, Coin (..), HeaderHash, decodeTextAddress)
import           Pos.Crypto (decodeHash)
import           Pos.Txp (TxId, TxIn (..), TxOut (..), TxOutAux (..))

decodeBlkHash :: Text -> Maybe HeaderHash
decodeBlkHash = rightToMaybe . decodeHash

decodeTxHash :: Text -> Maybe TxId
decodeTxHash = rightToMaybe . decodeHash

decodeAddress :: Text -> Maybe Address
decodeAddress = rightToMaybe . decodeTextAddress

--FIXME: Move to UtxosTable?
toTxIn :: Text -> Int -> Maybe TxIn
toTxIn txHash idx = do
  decTxHash <- decodeTxHash txHash
  pure $ TxInUtxo decTxHash (fromIntegral idx)

toTxOut :: Text -> Int64 -> Maybe TxOutAux
toTxOut receiver amount = do
  decReceiver <- decodeAddress receiver
  pure $ TxOutAux $ TxOut decReceiver (Coin $ fromIntegral amount)

