-- | This module is testing the server api.

{-# LANGUAGE TypeFamilies #-}

module Test.Pos.BlockchainImporter.Web.ServerSpec
       ( spec
       ) where

import           Universum

import           Test.Hspec (Spec, describe, shouldBe)
import           Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import           Test.QuickCheck (arbitrary, counterexample, forAll, (==>))
import           Test.QuickCheck.Monadic (assert, monadicIO, run)

import           Pos.Arbitrary.Block ()
import           Pos.BlockchainImporter.BlockchainImporterMode (runBlockchainImporterTestMode)
import           Pos.BlockchainImporter.ExtraContext (ExtraContext (..), makeExtraCtx,
                                                      makeMockExtraCtx)
import           Pos.BlockchainImporter.TestUtil (emptyBlk,
                                                  generateValidBlockchainImporterMockableMode,
                                                  generateValidBlocksSlotsNumber, leftToCounter)
import           Pos.BlockchainImporter.Web.Server (getBlockDifficulty, getBlocksTotal)
import qualified Pos.Communication ()
import           Pos.Core (HasConfiguration)
import           Pos.Launcher.Configuration (HasConfigurations)
import           Pos.Util (divRoundUp)
-- Orphan mockable instances.
import           Pos.Util.Mockable ()
import           Test.Pos.Configuration (withDefConfigurations)


----------------------------------------------------------------
-- Spec
----------------------------------------------------------------

{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}

-- stack test cardano-sl-blockchain-importer --fast --test-arguments "-m Pos.BlockchainImporter.Web.Server"
spec :: Spec
spec = withDefConfigurations $ \_ -> do
    describe "Pos.BlockchainImporter.Web.Server" $ do
        blocksTotalSpec
        blocksPagesTotalSpec

        blocksTotalUnitSpec

        blocksTotalFunctionalSpec

-- | A spec with the following test invariant. If a block is generated, there is no way
-- that blocksTotal could be less than 1.
blocksTotalSpec :: HasConfigurations => Spec
blocksTotalSpec =
    describe "getBlockDifficulty"
    $ modifyMaxSuccess (const 200) $ do
        prop "created blocks means block size > 0" $ emptyBlk $ \blk0 -> leftToCounter blk0 $ \blk ->
            let mainBlock   = Right blk
                blocksTotal = getBlockDifficulty mainBlock
            in counterexample ("Total blocks sizes: " <> show blocksTotal <>
                               "\n\nBlock: " <> show blk) $
                 blocksTotal > 0

-- | A spec with the simple test that @getBlocksPagesTotal@ works correct.
-- It shows that two equal algorithms should work the same.
blocksPagesTotalSpec :: HasConfiguration => Spec
blocksPagesTotalSpec =
    describe "divRoundUp"
    $ modifyMaxSuccess (const 10000) $ do
        prop "valid page number holds" $
            forAll arbitrary $ \(blocksTotal :: Integer, pageSizeInt :: Integer) ->
            -- Otherwise it doesn't make sense.
            blocksTotal >= 1 && pageSizeInt >= 1 ==>
                ((blocksTotal - 1) `div` pageSizeInt) + 1
                    `shouldBe`
                        divRoundUp blocksTotal pageSizeInt

-- | A spec with the following test invariant. If a block is generated, there is no way
-- that blocksTotal could be different then the number of blocks on the blockchain.
-- I originally thought that this is going to be faster then emulation, but seems the
-- real issue of performance here is the block creation speed.
blocksTotalUnitSpec :: HasConfigurations => Spec
blocksTotalUnitSpec =
    describe "getBlocksTotal"
    $ modifyMaxSuccess (const 200) $ do
        prop "block total == created number of blockchain blocks" $
            forAll arbitrary $ \(testParams) ->
            forAll generateValidBlocksSlotsNumber $ \(totalBlocksNumber, slotsPerEpoch) ->

                monadicIO $ do

                  -- We replace the "real" blockchain with our custom generated one.
                  mode <- lift $ generateValidBlockchainImporterMockableMode totalBlocksNumber slotsPerEpoch

                  -- The extra context so we can mock the functions.
                  let extraContext :: ExtraContext
                      extraContext = makeMockExtraCtx mode

                  -- We run the function in @BlockTestMode@ so we don't need to define
                  -- a million instances.
                  let blockExecution :: IO Integer
                      blockExecution = runBlockchainImporterTestMode testParams extraContext getBlocksTotal

                  -- We finally run it as @PropertyM@ and check if it holds.
                  blocksTotal <- run blockExecution

                  -- And we assert that the generated blockchain total block count is equal
                  -- to the expected blockchainImporter API result.
                  assert $ blocksTotal == fromIntegral totalBlocksNumber


-- | A spec with the following test invariant. If a block is generated, there is no way
-- that blocksTotal could be less than 0.
-- We don't have control over this block generation, so it really can be 0.
blocksTotalFunctionalSpec :: HasConfigurations => Spec
blocksTotalFunctionalSpec =
    describe "getBlocksTotalFunctional"
    $ modifyMaxSuccess (const 200) $ do
        prop "created blocks means block size >= 0" $
            forAll arbitrary $ \testParams ->
                monadicIO $ do

                  -- The extra context so we can mock the functions.
                  let extraContext :: ExtraContext
                      extraContext = makeExtraCtx

                  -- We run the function in @BlockchainImporterTestMode@ so we don't need to define
                  -- a million instances.
                  let blockExecution :: IO Integer
                      blockExecution =
                          runBlockchainImporterTestMode testParams extraContext getBlocksTotal

                  -- We finally run it as @PropertyM@ and check if it holds.
                  blocksTotal <- run blockExecution
                  assert $ blocksTotal >= 0


