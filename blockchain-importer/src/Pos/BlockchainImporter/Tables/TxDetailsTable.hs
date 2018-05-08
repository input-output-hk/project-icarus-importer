{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

-- TODO: Add documentation.

module Pos.BlockchainImporter.Tables.TxDetailsTable
  ( insertTxDetails
  ) where

import           Control.Monad (void)
import qualified Data.List.NonEmpty as NE (toList)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import qualified Database.PostgreSQL.Simple as PGS
import           Opaleye
import           Pos.BlockchainImporter.Core (TxExtra (..))
import           Pos.BlockchainImporter.Tables.Utils (addressToString, hashToString)
import           Pos.Core (Coin (..))
import           Pos.Core.Txp (Tx (..), TxOut (..), TxOutAux (..))
import           Pos.Crypto (hash)
import           Universum

data TxDetailsRowPoly a b c d = TxDetailsRow { tdHash    :: a
                                             , tdIsInput :: b
                                             , tdAddress :: c
                                             , tdAmount  :: d
                                             } deriving (Show)

type TxDetailsRowPGW = TxDetailsRowPoly (Column PGText) (Column PGBool) (Column PGText) (Column PGInt8)
type TxDetailsRowPGR = TxDetailsRowPoly (Column PGText) (Column PGBool) (Column PGText) (Column PGInt8)

$(makeAdaptorAndInstance "pTxDetails" ''TxDetailsRowPoly)

txDetailsTable :: Table TxDetailsRowPGW TxDetailsRowPGR
txDetailsTable = Table "tx_details" (pTxDetails TxDetailsRow { tdHash    = required "hash"
                                                             , tdIsInput = required "is_input"
                                                             , tdAddress = required "address"
                                                             , tdAmount  = required "amount"
                                                             })

makeRowPGW :: String -> Bool -> TxOut -> TxDetailsRowPGW
makeRowPGW txHash isInput txOut = TxDetailsRow {..}
  where
    tdHash          = pgString txHash
    tdIsInput       = pgBool isInput
    tdAddress       = pgString address
    tdAmount        = pgInt8 amount
    amount          = fromIntegral $ getCoin coin
    (address, coin) = (addressToString . txOutAddress &&& txOutValue) txOut

insertTxDetails :: PGS.Connection -> Tx -> TxExtra -> IO ()
insertTxDetails conn tx txExtra = do
  -- FIXME: Remove, only for debugging purposes.
  --putStrLn $ "--------- inputs: "  ++ show inputs
  --putStrLn $ "--------- outputs: " ++ show outputs
  void $ runInsertMany conn txDetailsTable rows
  where
    txHash           = hashToString (hash tx)
    rows             = inputs ++ outputs
    inputs           = makeRows True $ mapMaybe (fmap toaOut) (NE.toList $ teInputOutputs txExtra)
    outputs          = makeRows False $ NE.toList $ _txOutputs tx
    makeRows isInput = map (makeRowPGW txHash isInput)
