module Bench.Pos.BlockchainImporter.ServerBench
    ( runTimeBenchmark
    , runSpaceBenchmark
    ) where

import           Universum

import           Criterion.Main (bench, defaultConfig, defaultMainWith, nfIO)
import           Criterion.Types (Config (..))
import           Weigh (io, mainWith)

import           Test.QuickCheck (arbitrary, generate)

import           Pos.Arbitrary.Txp.Unsafe ()

import           Test.Pos.Configuration (withDefConfigurations)

import           Pos.BlockchainImporter.BlockchainImporterMode (BlockchainImporterTestParams,
                                                                runBlockchainImporterTestMode)
import           Pos.BlockchainImporter.Configuration (withPostGresDB)
import           Pos.BlockchainImporter.ExtraContext (ExtraContext (..), makeMockExtraCtx)
import           Pos.BlockchainImporter.TestUtil (BlockNumber, SlotsPerEpoch,
                                                  generateValidBlockchainImporterMockableMode)
import           Pos.BlockchainImporter.Web.Server (getBlocksTotal)


----------------------------------------------------------------
-- Mocked functions
----------------------------------------------------------------

type BenchmarkTestParams = (BlockchainImporterTestParams, ExtraContext)

-- | @getBlocksTotal@ function for benchmarks.
getBlocksTotalBench
    :: BenchmarkTestParams
    -> IO Integer
getBlocksTotalBench (testParams, extraContext) =
    withDefConfigurations $ \_ ->
      -- Postgres db is not mocked as it's never used
      withPostGresDB (error "No postgres db configured") 0 $
        runBlockchainImporterTestMode testParams extraContext getBlocksTotal

-- | This is used to generate the test environment. We don't do this while benchmarking
-- the functions since that would include the time/memory required for the generation of the
-- mock blockchain (test environment), and we don't want to include that in our benchmarks.
generateTestParams
    :: BlockNumber
    -> SlotsPerEpoch
    -> IO BenchmarkTestParams
generateTestParams totalBlocksNumber slotsPerEpoch = do
    testParams <- testParamsGen

    -- We replace the "real" blockchain with our custom generated one.
    mode <- generateValidBlockchainImporterMockableMode totalBlocksNumber slotsPerEpoch

    -- The extra context so we can mock the functions.
    -- Postgres db is not mocked as it's never used
    let extraContext :: ExtraContext
        extraContext = withPostGresDB (error "No postgres db configured") 0 $
                          withDefConfigurations $ const $ makeMockExtraCtx mode

    pure (testParams, extraContext)
  where
    -- | Generated test parameters.
    testParamsGen :: IO BlockchainImporterTestParams
    testParamsGen = generate arbitrary

-- | Extracted common code. This needs to be run before the benchmarks since we don't
-- want to include time/memory of the test data generation in the benchmarks.
usingGeneratedBlocks :: IO (BenchmarkTestParams, BenchmarkTestParams, BenchmarkTestParams)
usingGeneratedBlocks = do

    blocks100   <- generateTestParams 100 10
    blocks1000  <- generateTestParams 1000 10
    blocks10000 <- generateTestParams 10000 10

    pure (blocks100, blocks1000, blocks10000)

----------------------------------------------------------------
-- Time benchmark
----------------------------------------------------------------

-- | Time @getBlocksPage@.
runTimeBenchmark :: IO ()
runTimeBenchmark = do
    -- Generate the test environment before the benchmarks.
    (blocks100, blocks1000, blocks10000) <- usingGeneratedBlocks

    defaultMainWith getBlocksPageConfig
        [ bench "getBlocksTotal 100 blocks" $ nfIO $ getBlocksTotalBench blocks100
        , bench "getBlocksTotal 1000 blocks" $ nfIO $ getBlocksTotalBench blocks1000
        , bench "getBlocksTotal 10000 blocks" $ nfIO $ getBlocksTotalBench blocks10000
        ]

  where
    -- | Configuration.
    getBlocksPageConfig :: Config
    getBlocksPageConfig = defaultConfig
        { reportFile = Just "bench/results/ServerBackend.html"
        }

----------------------------------------------------------------
-- Space benchmark
----------------------------------------------------------------

-- | Space @getBlocksPage@.
runSpaceBenchmark :: IO ()
runSpaceBenchmark = do
    -- Generate the test environment before the benchmarks.
    (blocks100, blocks1000, blocks10000) <- usingGeneratedBlocks

    mainWith $ do
        io "getBlocksTotal 100 blocks" getBlocksTotalBench blocks100
        io "getBlocksTotal 1000 blocks" getBlocksTotalBench blocks1000
        io "getBlocksTotal 10000 blocks" getBlocksTotalBench blocks10000
