{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}

module Main
       ( main
       ) where

import           Universum

import           Data.Maybe (fromJust)
import qualified Database.PostgreSQL.Simple as PGS
import           Mockable (Production, runProduction)
import           System.Wlog (LoggerName, logInfo)

import           Pos.Binary ()
import           Pos.BlockchainImporter.Configuration (HasPostGresDB, withPostGresDB)
import           Pos.BlockchainImporter.ExtraContext (makeExtraCtx)
import           Pos.BlockchainImporter.Txp (BlockchainImporterExtraModifier,
                                             blockchainImporterTxpGlobalSettings)
import           Pos.BlockchainImporter.Web (BlockchainImporterProd, runBlockchainImporterProd)
import           Pos.Client.CLI (CommonNodeArgs (..), NodeArgs (..), getNodeParams)
import qualified Pos.Client.CLI as CLI
import           Pos.Core (HeaderHash)
import           Pos.DB.DB (initNodeDBs)
import           Pos.Launcher (ConfigurationOptions (..), HasConfigurations, NodeParams (..),
                               NodeResources (..), bracketNodeResources, elimRealMode,
                               loggerBracket, withConfigurations)
import           Pos.PostgresConsistency.ConsistencyChecker
import           Pos.PostgresConsistency.Utils (decodeBlkHash)
import           Pos.Util (logException)
import           Pos.Util.CompileInfo (HasCompileInfo, retrieveCompileTimeInfo, withCompileInfo)
import           Pos.Util.UserSecret (usVss)
import           PostgresConsistencyNodeOptions (PostgresChecks (..), PostgresConsistencyArgs (..),
                                                 PostgresConsistencyNodeArgs (..),
                                                 getPostgresConsistencyNodeOptions)

loggerName :: LoggerName
loggerName = "node"

----------------------------------------------------------------------------
-- Main action
----------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getPostgresConsistencyNodeOptions
    let loggingParams = CLI.loggingParams loggerName (enaCommonNodeArgs args)
    loggerBracket loggingParams . logException "node" . runProduction $ do
        logInfo "[Attention] Software is built with blockchainImporter part"
        action args

action :: PostgresConsistencyNodeArgs -> Production ()
action (PostgresConsistencyNodeArgs (cArgs@CommonNodeArgs{..}) PostgresConsistencyArgs{..}) =
    withConfigurations conf $ \ntpConfig -> do
      conn <- liftIO $ PGS.connect postGresConfig
      withPostGresDB conn storingStartBlockPG $
        withCompileInfo $(retrieveCompileTimeInfo) $ do
            CLI.printInfoOnStart cArgs ntpConfig
            logInfo "Blockchain importer is enabled!"
            currentParams <- getNodeParams loggerName cArgs nodeArgs

            let vssSK = fromJust $ npUserSecret currentParams ^. usVss
                sscParams = CLI.gtSscParams cArgs vssSK (npBehaviorConfig currentParams)

            bracketNodeResources currentParams sscParams
              blockchainImporterTxpGlobalSettings initNodeDBs $ \nr ->
                runBlockchainImporterRealMode nr
  where
    runBlockchainImporterRealMode
        :: (HasConfigurations, HasCompileInfo, HasPostGresDB)
        => NodeResources BlockchainImporterExtraModifier
        -> Production ()
    runBlockchainImporterRealMode nr@NodeResources{..} =
        let extraCtx = makeExtraCtx
            blockchainImporterModeToRealMode  = runBlockchainImporterProd extraCtx
            elim = elimRealMode nr
            consistencyCheckerRealMode = blockchainImporterModeToRealMode $
              callSelectedCheck checksToDo
        in  elim consistencyCheckerRealMode


    conf :: ConfigurationOptions
    conf = CLI.configurationOptions $ CLI.commonArgs cArgs

    nodeArgs :: NodeArgs
    nodeArgs = NodeArgs { behaviorConfigPath = Nothing }

    getBlkHashes :: String -> IO [HeaderHash]
    getBlkHashes fileName = do
      contents <- readFile fileName
      pure $ catMaybes $ decodeBlkHash <$> words contents

    callSelectedCheck ::
         (HasConfigurations, HasCompileInfo, HasPostGresDB)
      => PostgresChecks
      -> BlockchainImporterProd ()
    callSelectedCheck checkSelected = case checkSelected of
      ExternalConsistency blkHashFile -> do
        blksToCheck <- liftIO $ getBlkHashes blkHashFile
        checkRes <- externalConsistency blksToCheck
        logCheckResult checkRes
      InternalConsistency -> do
        checkRes <- internalConsistencyCheck
        logCheckResult checkRes
      ExternalTxRangeConsistency tipHash -> do
        checkRes <- externalConsistencyWithTxRange $ fromJust $ decodeBlkHash $ toText tipHash
        logCheckResult checkRes
      GetTipHash -> printTipHash

    logCheckResult :: Bool -> BlockchainImporterProd ()
    logCheckResult result =
      case result of
        True  -> logInfo "Consistency check succeeded"
        False -> logInfo "Consistency check failed"
