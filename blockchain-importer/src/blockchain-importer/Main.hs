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

import           BlockchainImporterNodeOptions (BlockchainImporterArgs (..),
                                                BlockchainImporterNodeArgs (..),
                                                getBlockchainImporterNodeOptions)
import           Pos.Binary ()
import           Pos.BlockchainImporter.Configuration (withPostGresDB)
import           Pos.BlockchainImporter.DB (blockchainImporterInitDB)
import           Pos.BlockchainImporter.ExtraContext (makeExtraCtx)
import           Pos.BlockchainImporter.Txp (BlockchainImporterExtraModifier,
                                             blockchainImporterTxpGlobalSettings)
import           Pos.BlockchainImporter.Web (BlockchainImporterProd, blockchainImporterPlugin,
                                             runBlockchainImporterProd)
import           Pos.Client.CLI (CommonNodeArgs (..), NodeArgs (..), getNodeParams)
import qualified Pos.Client.CLI as CLI
import           Pos.Context (NodeContext (..))
import           Pos.Diffusion.Types (Diffusion)
import           Pos.Launcher (ConfigurationOptions (..), HasConfigurations, NodeParams (..),
                               NodeResources (..), bracketNodeResources, elimRealMode,
                               loggerBracket, runNode, runServer, withConfigurations)
import           Pos.Reporting.Ekg (EkgNodeMetrics (..))
import           Pos.Update.Worker (updateTriggerWorker)
import           Pos.Util (logException)
import           Pos.Util.CompileInfo (HasCompileInfo, retrieveCompileTimeInfo, withCompileInfo)
import           Pos.Util.UserSecret (usVss)

loggerName :: LoggerName
loggerName = "node"

----------------------------------------------------------------------------
-- Main action
----------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getBlockchainImporterNodeOptions
    let loggingParams = CLI.loggingParams loggerName (enaCommonNodeArgs args)
    loggerBracket loggingParams . logException "node" . runProduction $ do
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
            bracketNodeResources currentParams sscParams
                blockchainImporterTxpGlobalSettings
                blockchainImporterInitDB $ \nr@NodeResources {..} ->
                  runBlockchainImporterRealMode nr (runNode nr plugins)
  where

    conf :: ConfigurationOptions
    conf = CLI.configurationOptions $ CLI.commonArgs cArgs

    runBlockchainImporterRealMode
        :: (HasConfigurations,HasCompileInfo)
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
            serverRealMode = blockchainImporterModeToRealMode $ runServer
                (runProduction . elim . blockchainImporterModeToRealMode)
                ncNodeParams
                ekgNodeMetrics
                go
        in  elim serverRealMode

    nodeArgs :: NodeArgs
    nodeArgs = NodeArgs { behaviorConfigPath = Nothing }
