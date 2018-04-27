
-- | Tests of Pos.BlockchainImporter.Socket.Util

module Test.Pos.BlockchainImporter.Socket.UtilSpec
       ( spec
       ) where

import           Universum

import qualified Data.IntSet as IS
import qualified Data.Map as M
import           Pos.BlockchainImporter.Socket.Util (regroupBySnd)
import           Test.Hspec (Spec, describe, it, shouldBe)



----------------------------------------------------------------------------
-- Spec
----------------------------------------------------------------------------

-- stack test cardano-sl-blockchain-importer --fast --test-arguments "-m Test.Pos.BlockchainImporter.Socket"

spec :: Spec
spec =
    describe "Util" $
        describe "regroupBySnd" $
            it "re-grouped a list by second value of its elements" $ do
                let result = regroupBySnd [ (IS.singleton 1, "1" :: Text)
                                          , (IS.singleton 2, "1" :: Text)
                                          , (IS.singleton 3, "2" :: Text)
                                          , (IS.singleton 4, "3" :: Text)
                                          , (IS.singleton 5, "3" :: Text)
                                          ]
                    expected =  M.fromList  [ ('1', [ IS.singleton 2
                                                    , IS.singleton 1 ])
                                            , ('2', [ IS.singleton 3 ])
                                            , ('3', [ IS.singleton 5
                                                    , IS.singleton 4 ])
                                            ]
                result `shouldBe` expected
