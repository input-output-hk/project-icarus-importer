{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Pos.BlockchainImporter.Tables.BestBlockTable
  ( -- * Data manipulation
    updateBestBlock
  ) where

import           Universum

import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import qualified Database.PostgreSQL.Simple as PGS
import           Opaleye

data BestBlockRowPoly a = BestBlockRow  { bbBlockNum :: a
                                        } deriving (Show)

type BestBlockRowPGW = BestBlockRowPoly (Column PGInt8)
type BestBlockRowPGR = BestBlockRowPoly (Column PGInt8)

$(makeAdaptorAndInstance "pBestBlock" ''BestBlockRowPoly)

bestBlockTable :: Table BestBlockRowPGW BestBlockRowPGR
bestBlockTable = Table "bestblock" (pBestBlock  BestBlockRow
                                                { bbBlockNum = required "best_block_num" })

updateBestBlock :: PGS.Connection -> Word64 -> IO ()
updateBestBlock conn newBestBlock = do
  n <- runUpdate conn bestBlockTable (const colBlockNum) (const $ pgBool True)
  when (n == 0) $ void $ runInsertMany conn bestBlockTable [colBlockNum]
    where colBlockNum = BestBlockRow $ pgInt8 $ fromIntegral newBestBlock
