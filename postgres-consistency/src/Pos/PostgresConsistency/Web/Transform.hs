{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Pos.PostgresConsistency.Web.Transform
       ( BlockchainImporterProd
       , runBlockchainImporterProd
       , liftToBlockchainImporterProd
       , blockchainImporterServeWebReal
       , blockchainImporterPlugin
       ) where

import           Universum

import qualified Control.Exception.Safe as E
import           Control.Monad.Except (MonadError (throwError))
import qualified Control.Monad.Reader as Mtl
import           Mockable (runProduction)
import           Servant.Server (Handler, hoistServer)

import           Pos.Block.Configuration (HasBlockConfiguration)
import           Pos.Configuration (HasNodeConfiguration)
import           Pos.Core (HasConfiguration)
import           Pos.Diffusion.Types (Diffusion)
import           Pos.Recovery ()
import           Pos.Ssc.Configuration (HasSscConfiguration)
import           Pos.Txp (HasTxpConfiguration, MempoolExt, MonadTxpLocal (..))
import           Pos.Update.Configuration (HasUpdateConfiguration)
import           Pos.Util.CompileInfo (HasCompileInfo)
import           Pos.Util.Mockable ()
import           Pos.WorkMode (RealMode, RealModeContext (..))

import           Pos.PostgresConsistency.BListener (BlockchainImporterBListener,
                                                    runBlockchainImporterBListener)
import           Pos.PostgresConsistency.Configuration (HasPostGresDB)
import           Pos.PostgresConsistency.ExtraContext (ExtraContext, ExtraContextT, makeExtraCtx,
                                                       runExtraContextT)
import           Pos.PostgresConsistency.Txp (BlockchainImporterExtraModifier, eTxNormalize,
                                              eTxProcessTransaction)
import           Pos.PostgresConsistency.Web.Api (blockchainImporterApi)
import           Pos.PostgresConsistency.Web.Server (blockchainImporterApp,
                                                     blockchainImporterHandlers,
                                                     blockchainImporterServeImpl)

-----------------------------------------------------------------
-- Transformation to `Handler`
-----------------------------------------------------------------

type RealModeE = RealMode BlockchainImporterExtraModifier
type BlockchainImporterProd = ExtraContextT (BlockchainImporterBListener RealModeE)

type instance MempoolExt BlockchainImporterProd = BlockchainImporterExtraModifier

instance (HasConfiguration, HasTxpConfiguration, HasCompileInfo, HasPostGresDB) =>
         MonadTxpLocal RealModeE where
    txpNormalize = eTxNormalize
    txpProcessTx = eTxProcessTransaction

instance (HasConfiguration, HasTxpConfiguration, HasCompileInfo, HasPostGresDB) =>
         MonadTxpLocal BlockchainImporterProd where
    txpNormalize = lift $ lift txpNormalize
    txpProcessTx = lift . lift . txpProcessTx

runBlockchainImporterProd :: ExtraContext -> BlockchainImporterProd a -> RealModeE a
runBlockchainImporterProd extraCtx = runBlockchainImporterBListener . runExtraContextT extraCtx

liftToBlockchainImporterProd :: RealModeE a -> BlockchainImporterProd a
liftToBlockchainImporterProd = lift . lift

type HasBlockchainImporterConfiguration =
    ( HasConfiguration
    , HasBlockConfiguration
    , HasNodeConfiguration
    , HasUpdateConfiguration
    , HasSscConfiguration
    , HasTxpConfiguration
    , HasPostGresDB
    , HasCompileInfo
    )

blockchainImporterPlugin
    :: HasBlockchainImporterConfiguration
    => Word16
    -> Diffusion BlockchainImporterProd
    -> BlockchainImporterProd ()
blockchainImporterPlugin = flip blockchainImporterServeWebReal

blockchainImporterServeWebReal
    :: HasBlockchainImporterConfiguration
    => Diffusion BlockchainImporterProd
    -> Word16
    -> BlockchainImporterProd ()
blockchainImporterServeWebReal diffusion port = do
    rctx <- ask
    let handlers = blockchainImporterHandlers diffusion
        server = hoistServer blockchainImporterApi (convertHandler rctx) handlers
        app = blockchainImporterApp (pure server)
    blockchainImporterServeImpl app port

convertHandler
    :: HasConfiguration
    => RealModeContext BlockchainImporterExtraModifier
    -> BlockchainImporterProd a
    -> Handler a
convertHandler rctx handler =
    let extraCtx = makeExtraCtx
        ioAction = realRunner $
                   runBlockchainImporterProd extraCtx
                   handler
    in liftIO ioAction `E.catches` excHandlers
  where
    realRunner :: forall t . RealModeE t -> IO t
    realRunner act = runProduction $ Mtl.runReaderT act rctx

    excHandlers = [E.Handler catchServant]
    catchServant = throwError
