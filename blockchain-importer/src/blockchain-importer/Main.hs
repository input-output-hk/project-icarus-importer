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
import           System.Wlog (LoggerName, logError, logInfo, logWarning)

import           BlockchainImporterNodeOptions (BlockchainImporterArgs (..),
                                                BlockchainImporterNodeArgs (..),
                                                getBlockchainImporterNodeOptions)
import           Pos.Binary ()
import           Pos.BlockchainImporter.Configuration (HasPostGresDB, withPostGreTransaction,
                                                       withPostGresDB)
import           Pos.BlockchainImporter.ExtraContext (makeExtraCtx)
import           Pos.BlockchainImporter.Recovery (recoverDBsConsistency)
import           Pos.BlockchainImporter.Tables.TxsTable (markPendingTxsAsFailed)
import           Pos.BlockchainImporter.Txp (BlockchainImporterExtraModifier,
                                             blockchainImporterTxpGlobalSettings)
import           Pos.BlockchainImporter.Web (BlockchainImporterProd, blockchainImporterPlugin,
                                             runBlockchainImporterProd)
import           Pos.Client.CLI (CommonNodeArgs (..), NodeArgs (..), getNodeParams)
import qualified Pos.Client.CLI as CLI
import           Pos.Context (NodeContext (..))
import           Pos.DB.DB (initNodeDBs)
import           Pos.Diffusion.Types (Diffusion)
import           Pos.ImporterDBConsistency.ConsistencyChecker (internalConsistencyCheck)
import           Pos.Launcher (ConfigurationOptions (..), HasConfigurations, NodeParams (..),
                               NodeResources (..), bracketNodeResources, elimRealMode,
                               loggerBracket, runNode, runServer, withConfigurations)
import           Pos.Reporting.Ekg (EkgNodeMetrics (..))
import           Pos.Update.Worker (updateTriggerWorker)
import           Pos.Util (logException)
import           Pos.Util.CompileInfo (HasCompileInfo, retrieveCompileTimeInfo, withCompileInfo)
import           Pos.Util.UserSecret (usVss)

loggerName :: LoggerName
loggerName = "importer"

----------------------------------------------------------------------------
-- Main action
----------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getBlockchainImporterNodeOptions
    let loggingParams = CLI.loggingParams loggerName (enaCommonNodeArgs args)
    loggerBracket loggingParams . logException "importer" . runProduction $ do
        logInfo "[Attention] Software is built with blockchainImporter part"
        action args

action :: BlockchainImporterNodeArgs -> Production ()
action (BlockchainImporterNodeArgs (cArgs@CommonNodeArgs{..}) BlockchainImporterArgs{..}) =
    withConfigurations conf $ \ntpConfig -> do
      conn <- liftIO $ PGS.connect postGresConfig
      withPostGresDB conn storingStartBlockPG $
        withCompileInfo $(retrieveCompileTimeInfo) $ do
            CLI.printInfoOnStart cArgs ntpConfig
            logInfo "Blockchain importer is enabled!"
            currentParams <- getNodeParams loggerName cArgs nodeArgs

            let vssSK = fromJust $ npUserSecret currentParams ^. usVss
            let sscParams = CLI.gtSscParams cArgs vssSK (npBehaviorConfig currentParams)

            let plugins :: HasConfigurations => [Diffusion BlockchainImporterProd -> BlockchainImporterProd ()]
                plugins =
                    [ blockchainImporterPlugin webPort
                    , updateTriggerWorker
                    ]

            -- Mark all pending txs as failed
            -- Note: During the syncing process some of these txs could change their state
            --       to successful. This is not a problem as it only happens in the rare case
            --       of a restart of the importer, which should be properly informed to the user.
            liftIO $ withPostGreTransaction $ markPendingTxsAsFailed conn

            bracketNodeResources currentParams sscParams
                blockchainImporterTxpGlobalSettings
                initNodeDBs $ \nr@NodeResources {..} ->
                  runBlockchainImporterRealMode nr (runNode nr plugins)
  where

    conf :: ConfigurationOptions
    conf = CLI.configurationOptions $ CLI.commonArgs cArgs

    runBlockchainImporterRealMode
        :: (HasConfigurations,HasCompileInfo, HasPostGresDB)
        => NodeResources BlockchainImporterExtraModifier
        -> (Diffusion BlockchainImporterProd -> BlockchainImporterProd ())
        -> Production ()
    runBlockchainImporterRealMode nr@NodeResources{..} go =
        let NodeContext {..} = nrContext
            extraCtx = makeExtraCtx
            blockchainImporterModeToRealMode  = runBlockchainImporterProd extraCtx
            elim = elimRealMode nr
            ekgNodeMetrics = EkgNodeMetrics
                nrEkgStore
            startImporter = runServer
              (runProduction . elim . blockchainImporterModeToRealMode)
              ncNodeParams
              ekgNodeMetrics
              go
            serverRealMode = blockchainImporterModeToRealMode $
              if recoveryMode then do
                  recoverDBsConsistency
                  startImporter
              else if disableConsistencyCheck then do
                logWarning "Initial consistency check disabled!"
                startImporter
              else do
                logInfo "Checking internal db consistency..."
                consistentDBs <- internalConsistencyCheck
                if consistentDBs then do
                  logInfo "Consistency check succeded, starting importer..."
                  startImporter
                else logError $ toText $ "Inconsistency detected between Rocks and " ++
                                         "Postgres DBs, start using --recovery-mode flag"
        in  elim serverRealMode

    nodeArgs :: NodeArgs
    nodeArgs = NodeArgs { behaviorConfigPath = Nothing }
