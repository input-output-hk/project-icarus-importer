{-# LANGUAGE TypeFamilies #-}

module Pos.PostgresConsistency.BlockchainImporterMode
    ( -- PostgresConsistency
      BlockchainImporterMode
    , BlockchainImporterTestMode
    , BlockchainImporterTestParams
    , runBlockchainImporterTestMode
    , etcParams_L
    , BlockchainImporterProperty
    , blockchainImporterPropertyToProperty
    ) where

import           Universum

import           Control.Lens (lens, makeLensesWith)
import           System.Wlog (HasLoggerName (..), LoggerName (..))

import           Test.QuickCheck (Gen, Property, Testable (..), arbitrary, forAll, ioProperty)
import           Test.QuickCheck.Monadic (PropertyM, monadic)

import           Pos.Block.Slog (mkSlogGState)
import           Pos.Core (SlotId, Timestamp (..))
import           Pos.DB (MonadGState (..))
import qualified Pos.DB as DB
import qualified Pos.DB.Block as DB
import           Pos.DB.Class (MonadDBRead)
import           Pos.DB.DB as DB
import qualified Pos.GState as GS
import           Pos.Lrc (LrcContext (..), mkLrcSyncData)
import           Pos.Slotting (HasSlottingVar (..), MonadSlots (..), MonadSlotsData,
                               SimpleSlottingStateVar, mkSimpleSlottingStateVar)
import qualified Pos.Slotting as Slot
import           Pos.Txp (GenericTxpLocalData (..), MempoolExt, MonadTxpMem, TxpHolderTag,
                          mkTxpLocalData)
import           Pos.Util (postfixLFields)
import           Pos.Util.Mockable ()
import           Pos.Util.Util (HasLens (..))

import           Pos.PostgresConsistency.ExtraContext (ExtraContext, ExtraContextT,
                                                       HasBlockchainImporterCSLInterface,
                                                       makeExtraCtx, runExtraContextT)
import           Pos.PostgresConsistency.Txp (BlockchainImporterExtraModifier (..))

-- Need Emulation because it has instance Mockable CurrentTime
import           Mockable (Production, currentTime, runProduction)
import           Pos.Launcher.Configuration (HasConfigurations)
import           Pos.Util.JsonLog.Events (HasJsonLogConfig (..), jsonLogDefault)
import           Pos.Util.LoggerName (HasLoggerName' (..), askLoggerNameDefault,
                                      modifyLoggerNameDefault)
import           Pos.Util.TimeWarp (CanJsonLog (..))
import           Pos.WorkMode (MinWorkMode)
import           Test.Pos.Block.Logic.Emulation (Emulation (..), runEmulation)
import           Test.Pos.Block.Logic.Mode (TestParams (..))


-------------------------------------------------------------------------------------
-- PostgresConsistency mode
-------------------------------------------------------------------------------------

-- | We require much less then @WorkMode@, and this simplifies things later when
-- testing (and running).
type BlockchainImporterMode ctx m =
    ( MonadDBRead m
    -- Database operations
    , MonadSlots ctx m
    -- Slotting
    , MonadThrow m
    , MonadCatch m
    , MonadMask m
    -- General utility operations
    , HasBlockchainImporterCSLInterface m
    -- Genesis operations
    , MonadTxpMem (MempoolExt m) ctx m
    -- Txp, could be @TxpLocalWorkMode@
    , MinWorkMode m
    -- The rest of the constraints - logger, mockable, configurations
    )

----------------------------------------------------------------------------
-- TestParams
----------------------------------------------------------------------------

-- An object of this type has instance Arbitrary and is used as the source
-- of randomness in the tests.
type BlockchainImporterTestParams = TestParams

----------------------------------------------------------------------------
-- Mock for BlockchainImporterTestMode
----------------------------------------------------------------------------

-- | Test mode to run with. Using @Emulation@.
type BlockchainImporterTestMode = ReaderT BlockchainImporterTestContext Emulation

-- | Test mode with extra context so we can mock out the external functions.
type BlockchainImporterExtraTestMode = ExtraContextT BlockchainImporterTestMode

data BlockchainImporterTestContext = BlockchainImporterTestContext
    { etcGState       :: !GS.GStateContext
    , etcSystemStart  :: !Timestamp
    , etcSSlottingVar :: !SimpleSlottingStateVar
    , etcSlotId       :: !(Maybe SlotId)
    -- ^ If this value is 'Just' we will return it as the current
    -- slot. Otherwise simple slotting is used.
    , etcTxpLocalData :: !(GenericTxpLocalData BlockchainImporterExtraModifier)
    , etcLoggerName   :: !LoggerName
    , etcParams       :: !BlockchainImporterTestParams
    }

makeLensesWith postfixLFields ''BlockchainImporterTestContext

instance HasLens SimpleSlottingStateVar BlockchainImporterTestContext SimpleSlottingStateVar where
    lensOf = etcSSlottingVar_L

----------------------------------------------------------------------------
-- Mock initialization
----------------------------------------------------------------------------

data BlockchainImporterTestInitContext = BlockchainImporterTestInitContext
    { eticDBPureVar      :: !DB.DBPureVar
    }

makeLensesWith postfixLFields ''BlockchainImporterTestInitContext

type BlockchainImporterTestInitMode = ReaderT BlockchainImporterTestInitContext Production

runTestInitMode :: BlockchainImporterTestInitContext -> BlockchainImporterTestInitMode a -> IO a
runTestInitMode ctx = runProduction . usingReaderT ctx

initBlockchainImporterTestContext
    :: (HasConfigurations, MonadIO m)
    => BlockchainImporterTestParams
    -> m BlockchainImporterTestContext
initBlockchainImporterTestContext tp@TestParams {..} = do
    dbPureVar <- DB.newDBPureVar
    let initCtx = BlockchainImporterTestInitContext
            { eticDBPureVar      = dbPureVar
            }
    liftIO $ runTestInitMode initCtx $ do
        DB.initNodeDBs
        lcLrcSync <- newTVarIO =<< mkLrcSyncData
        let _gscLrcContext = LrcContext {..}
        _gscSlogGState <- mkSlogGState
        _gscSlottingVar <- newTVarIO =<< GS.getSlottingData
        let etcGState = GS.GStateContext {_gscDB = DB.PureDB dbPureVar, ..}
        etcSSlottingVar <- mkSimpleSlottingStateVar
        etcSystemStart <- Timestamp <$> currentTime
        etcTxpLocalData <- mkTxpLocalData

        let etcSlotId       = Nothing
            etcParams       = tp
            etcLoggerName   = "blockchainImportertesting"
        pure BlockchainImporterTestContext {..}

-- | Run test mode with @ExtraContext@ so we can mock the functions.
runBlockchainImporterTestMode
    :: HasConfigurations
    => BlockchainImporterTestParams
    -> ExtraContext
    -> BlockchainImporterExtraTestMode a
    -> IO a
runBlockchainImporterTestMode tp extraContext action = do
    ctx <- initBlockchainImporterTestContext tp
    runEmulation (getTimestamp $ _tpStartTime tp) $
        usingReaderT ctx (runExtraContextT extraContext action)

----------------------------------------------------------------------------
-- Boilerplate BlockchainImporterTestInitContext instances
----------------------------------------------------------------------------

instance HasLens DB.DBPureVar BlockchainImporterTestInitContext DB.DBPureVar where
    lensOf = eticDBPureVar_L

----------------------------------------------------------------------------
-- Boilerplate BlockchainImporterTestInitMode instances
----------------------------------------------------------------------------

instance HasConfigurations => DB.MonadDBRead BlockchainImporterTestInitMode where
    dbGet = DB.dbGetPureDefault
    dbIterSource = DB.dbIterSourcePureDefault
    dbGetSerBlock = DB.dbGetSerBlockPureDefault
    dbGetSerUndo = DB.dbGetSerUndoPureDefault

instance HasConfigurations => DB.MonadDB BlockchainImporterTestInitMode where
    dbPut = DB.dbPutPureDefault
    dbWriteBatch = DB.dbWriteBatchPureDefault
    dbDelete = DB.dbDeletePureDefault
    dbPutSerBlunds = DB.dbPutSerBlundsPureDefault

----------------------------------------------------------------------------
-- Boilerplate BlockchainImporterTestContext instances
----------------------------------------------------------------------------

instance GS.HasGStateContext BlockchainImporterTestContext where
    gStateContext = etcGState_L

instance HasSlottingVar BlockchainImporterTestContext where
    slottingTimestamp = etcSystemStart_L
    slottingVar = GS.gStateContext . GS.gscSlottingVar

instance HasLens DB.DBPureVar BlockchainImporterTestContext DB.DBPureVar where
    lensOf = GS.gStateContext . GS.gscDB . pureDBLens
      where
        getter = \case
            DB.RealDB _   -> realDBInTestsError
            DB.PureDB pdb -> pdb
        setter _ pdb = DB.PureDB pdb
        pureDBLens = lens getter setter
        realDBInTestsError = error "You are using real db in tests"

-- We need to define the full transformer stack type.
type instance MempoolExt BlockchainImporterExtraTestMode = BlockchainImporterExtraModifier

instance HasLens TxpHolderTag BlockchainImporterTestContext (GenericTxpLocalData BlockchainImporterExtraModifier) where
    lensOf = etcTxpLocalData_L

instance HasLens LoggerName BlockchainImporterTestContext LoggerName where
      lensOf = etcLoggerName_L

instance HasLoggerName' BlockchainImporterTestContext where
    loggerName = lensOf @LoggerName

instance HasJsonLogConfig BlockchainImporterTestContext where
    jsonLogConfig = jsonLogConfig

----------------------------------------------------------------------------
-- Boilerplate BlockchainImporterTestMode instances
----------------------------------------------------------------------------

instance HasConfigurations => MonadGState BlockchainImporterTestMode where
    gsAdoptedBVData = DB.gsAdoptedBVDataDefault

instance (HasConfigurations, MonadSlotsData ctx BlockchainImporterTestMode)
      => MonadSlots ctx BlockchainImporterTestMode
  where
    getCurrentSlot = do
        view etcSlotId_L >>= \case
            Nothing -> Slot.getCurrentSlotSimple
            Just slot -> pure (Just slot)
    getCurrentSlotBlocking =
        view etcSlotId_L >>= \case
            Nothing -> Slot.getCurrentSlotBlockingSimple
            Just slot -> pure slot
    getCurrentSlotInaccurate = do
        view etcSlotId_L >>= \case
            Nothing -> Slot.getCurrentSlotInaccurateSimple
            Just slot -> pure slot
    currentTimeSlotting = Slot.currentTimeSlottingSimple

instance HasConfigurations => DB.MonadDBRead BlockchainImporterTestMode where
    dbGet = DB.dbGetPureDefault
    dbIterSource = DB.dbIterSourcePureDefault
    dbGetSerBlock = DB.dbGetSerBlockPureDefault
    dbGetSerUndo = DB.dbGetSerUndoPureDefault

instance HasConfigurations => DB.MonadDB BlockchainImporterTestMode where
    dbPut = DB.dbPutPureDefault
    dbWriteBatch = DB.dbWriteBatchPureDefault
    dbDelete = DB.dbDeletePureDefault
    dbPutSerBlunds = DB.dbPutSerBlundsPureDefault

instance {-# OVERLAPPING #-} HasLoggerName BlockchainImporterTestMode where
    askLoggerName = askLoggerNameDefault
    modifyLoggerName = modifyLoggerNameDefault

instance {-# OVERLAPPING #-} CanJsonLog BlockchainImporterTestMode where
    jsonLog = jsonLogDefault


----------------------------------------------------------------------------
-- Property
----------------------------------------------------------------------------

type BlockchainImporterProperty = PropertyM BlockchainImporterExtraTestMode

blockchainImporterPropertyToProperty
    :: HasConfigurations
    => Gen BlockchainImporterTestParams
    -> BlockchainImporterProperty a
    -> Property
blockchainImporterPropertyToProperty tpGen blockchainImporterTestProperty =
    forAll tpGen $ \tp ->
        monadic (ioProperty . (runBlockchainImporterTestMode tp makeExtraCtx)) blockchainImporterTestProperty

instance HasConfigurations => Testable (BlockchainImporterProperty a) where
    property = blockchainImporterPropertyToProperty arbitrary
