{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Pos.BlockchainImporter.Txp.Toil.UtxosTable where

import           Universum

import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Text.Lazy.Encoding as E (decodeUtf8)
import qualified Database.PostgreSQL.Simple as PGS
import           Formatting (sformat)
import           Opaleye
import qualified Pos.Binary.Class as Bi

import           Pos.Core.Common (Coin (..))
import           Pos.Core.Txp (TxIn (..), TxOut (..), TxOutAux (..))
import           Pos.Crypto (hashHexF)
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
hashToString h = toString $ sformat hashHexF h

toRecord :: TxIn -> TxOutAux -> UtxoRowPGW
toRecord (TxInUtxo txHash txIndex) (TxOutAux (TxOut receiver value)) = UtxoRow sHash iIndex sAddress iAmount
  where sHash     = pgString $ hashToString txHash
        iIndex    = pgInt4 $ fromIntegral txIndex
        sAddress  = pgLazyText $ E.decodeUtf8 $ Bi.serialize receiver
        iAmount   = pgInt8 $ fromIntegral $ getCoin value
toRecord _ _ = undefined --FIXME

-- FIXME: Replace with inserting a list of txs?
-- Inserts a tx into the table
applyModifierToUtxos :: PGS.Connection -> UtxoModifier -> IO ()
applyModifierToUtxos conn modifier = do
  let toInsert = (uncurry toRecord) <$> MM.insertions modifier
  rows <- runInsertMany conn utxosTable toInsert
  putStrLn $ show rows ++ " row(s) inserted (utxo's table)"  -- FIXME: Delete, for debugging purposes only
