{-# LANGUAGE Arrows #-}

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

updateBestBlock :: Word64 -> PGS.Connection -> IO ()
updateBestBlock newBestBlock conn = do
  n <- runUpdate_ conn $
                  Update bestBlockTable (const colBlockNum) (const $ pgBool True) rCount
  when (n == 0) $ void $ runInsert_ conn $
                                    Insert bestBlockTable [colBlockNum] rCount Nothing
    where colBlockNum = BestBlockRow $ pgInt8 $ fromIntegral newBestBlock
