{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Pos.BlockchainImporter.Tables.UtxosTable where

import           Universum

import           Data.Maybe (catMaybes)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import qualified Database.PostgreSQL.Simple as PGS
import           Opaleye

import           Pos.BlockchainImporter.Tables.Utils
import           Pos.Core.Common (Coin (..))
import           Pos.Core.Txp (TxIn (..), TxOut (..), TxOutAux (..))
import           Pos.Txp.Toil.Types (UtxoModifier)
import qualified Pos.Util.Modifier as MM

data UtxoRowPoly a b c d e = UtxoRow  { urUtxoId   :: a
                                      , urTxHash   :: b
                                      , urTxIndex  :: c
                                      , urReceiver :: d
                                      , urAmount   :: e
                                      } deriving (Show)

type UtxoRow = UtxoRowPoly String String Word32 String Word64
type UtxoRowPGW = UtxoRowPoly (Column PGText) (Column PGText) (Column PGInt4) (Column PGText) (Column PGInt8)
type UtxoRowPGR = UtxoRowPoly (Column PGText) (Column PGText) (Column PGInt4) (Column PGText) (Column PGInt8)

$(makeAdaptorAndInstance "pUtxos" ''UtxoRowPoly)

utxosTable :: Table UtxoRowPGW UtxoRowPGR
utxosTable = Table "utxos" (pUtxos UtxoRow  { urUtxoId = required "utxo_id"
                                            , urTxHash = required "tx_hash"
                                            , urTxIndex = required "tx_index"
                                            , urReceiver = required "receiver"
                                            , urAmount = required "amount"
                                            })

txId :: TxIn -> String
txId (TxInUtxo txHash txIndex) = hashToString txHash ++ show txIndex
txId _                         = ""

toRecord :: TxIn -> TxOutAux -> Maybe UtxoRowPGW
toRecord txIn@(TxInUtxo txHash txIndex) (TxOutAux (TxOut receiver value)) = Just $ row
  where sHash     = hashToString txHash
        iIndex    = fromIntegral txIndex
        sAddress  = addressToString receiver
        iAmount   = fromIntegral $ getCoin value
        row       = UtxoRow (pgString $ txId txIn)
                            (pgString sHash) (pgInt4 iIndex)
                            (pgString sAddress) (pgInt8 iAmount)
toRecord _ _ = Nothing

-- Applies a UtxoModifier to the UTxOs in the table
applyModifierToUtxos :: PGS.Connection -> UtxoModifier -> IO ()
applyModifierToUtxos conn modifier = do
  let toInsert = catMaybes $ (uncurry toRecord) <$> MM.insertions modifier
      toDelete = (pgString . txId) <$> MM.deletions modifier
  _ <- runInsertMany conn utxosTable toInsert
  _ <- runDelete conn utxosTable $ \(UtxoRow sId _ _ _ _) -> in_ toDelete sId
  return ()
