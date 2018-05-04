{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Pos.BlockchainImporter.Txp.Toil.UtxosTable where

import           Universum

import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import qualified Database.PostgreSQL.Simple as PGS
import           Formatting (sformat)
import           Opaleye

import           Pos.Core.Common (Address, Coin (..), addressF)
import           Pos.Core.Txp (TxIn (..), TxOut (..), TxOutAux (..))
import           Pos.Crypto (hashHexF)
import           Pos.Crypto.Hashing (AbstractHash)
import           Pos.Txp.Toil.Types (UtxoModifier)
import qualified Pos.Util.Modifier as MM

data UtxoRowPoly a b c d = UtxoRow  { urTxHash   :: a
                                    , urTxIndex  :: b
                                    , urReceiver :: c
                                    , urAmount   :: d
                                    } deriving (Show)

--FIXME: Remove String and text?
type UtxoRow = UtxoRowPoly String Word32 String Word64
type UtxoRowPGW = UtxoRowPoly (Column PGText) (Column PGInt4) (Column PGText) (Column PGInt8)
type UtxoRowPGR = UtxoRowPoly (Column PGText) (Column PGInt4) (Column PGText) (Column PGInt8)

$(makeAdaptorAndInstance "pUtxos" ''UtxoRowPoly)

utxosTable :: Table UtxoRowPGW UtxoRowPGR
utxosTable = Table "utxos" (pUtxos UtxoRow  { urTxHash = required "tx_hash"
                                            , urTxIndex = required "tx_index"
                                            , urReceiver = required "receiver"
                                            , urAmount = required "amount"
                                            })

-- FIXME: Move to utils?
hashToString :: AbstractHash algo a -> String
hashToString h = toString $ sformat hashHexF h

addressToString :: Address -> String
addressToString addr = toString $ sformat addressF addr

toRecord :: TxIn -> TxOutAux -> UtxoRowPGW
toRecord (TxInUtxo txHash txIndex) (TxOutAux (TxOut receiver value)) = UtxoRow sHash iIndex sAddress iAmount
  where sHash     = pgString $ hashToString txHash
        iIndex    = pgInt4 $ fromIntegral txIndex
        sAddress  = pgString $ addressToString receiver
        iAmount   = pgInt8 $ fromIntegral $ getCoin value
toRecord _ _ = undefined --FIXME

-- FIXME: Replace with inserting a list of txs?
-- Inserts a tx into the table
applyModifierToUtxos :: PGS.Connection -> UtxoModifier -> IO ()
applyModifierToUtxos conn modifier = do
  let toInsert = (uncurry toRecord) <$> MM.insertions modifier
  rows <- runInsertMany conn utxosTable toInsert
  putStrLn $ show rows ++ " row(s) inserted (utxo's table)"  -- FIXME: Delete, for debugging purposes only
