module Main
  ( main
  ) where

import           Universum

import           System.IO (hSetEncoding, stdout, utf8)

import qualified Bench.Pos.Explorer.ServerBench as SB

-- stack bench cardano-sl-blockchain-importer
main :: IO ()
main = do
    hSetEncoding stdout utf8

    SB.runTimeBenchmark
    -- SB.runSpaceBenchmark
