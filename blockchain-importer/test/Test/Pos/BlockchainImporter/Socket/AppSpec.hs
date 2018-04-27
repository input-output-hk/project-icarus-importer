
-- | Tests of Pos.BlockchainImporter.Socket.Util

module Test.Pos.BlockchainImporter.Socket.AppSpec
       ( spec
       ) where

import           Universum

import           Network.Wai.Handler.Warp (Settings, getPort)
import           Test.Hspec (Spec, describe, it, shouldBe)

import           Pos.BlockchainImporter.Socket.App (NotifierSettings (..), toConfig)
import           Test.Pos.BlockchainImporter.MockFactory (testLoggerName)

----------------------------------------------------------------------------
-- Spec
----------------------------------------------------------------------------

-- stack test cardano-sl-blockchain-importer --fast --test-arguments "-m Test.Pos.BlockchainImporter.Socket"

spec :: Spec
spec =
    describe "App" $
        describe "toConfig" $
            it "maps config into warp settings" $ do
                let p = 8200
                    ns = NotifierSettings { nsPort = p }
                    s :: Settings
                    s = toConfig ns testLoggerName
                -- currently we test port only,
                -- which is the only mapped property so far
                getPort s `shouldBe` fromIntegral p
