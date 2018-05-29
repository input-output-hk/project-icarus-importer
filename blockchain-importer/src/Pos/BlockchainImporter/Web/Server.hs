{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeOperators       #-}

-- API server logic

module Pos.BlockchainImporter.Web.Server
       ( blockchainImporterServeImpl
       , blockchainImporterApp
       , blockchainImporterHandlers

       -- pure functions
       , getBlockDifficulty
       , roundToBlockPage

       -- api functions
       , getBlocksTotal
       , getBlocksPagesTotal
       , getBlocksPage
       , getEpochSlot
       , getEpochPage

       -- function useful for socket-io server
       , topsortTxsOrFail
       , getMempoolTxs
       , getBlocksLastPage
       , getEpochPagesOrThrow
       , cAddrToAddr
       ) where

import           Universum

import           Control.Error.Util (exceptT, hoistEither)
import qualified Data.ByteString as BS
import           Data.Maybe (fromMaybe)
import           Data.Time.Units (Second)
import           Formatting (build, sformat, (%))
import           Mockable (Concurrently, Delay, Mockable, concurrently, delay)
import           Network.Wai (Application)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)

import qualified Serokell.Util.Base64 as B64
import           Servant.Generic (AsServerT, toServant)
import           Servant.Server (Server, ServerT, serve)

import           Pos.Crypto (WithHash (..), hash, redeemPkBuild)

import           Pos.Diffusion.Types (Diffusion (..))

import           Pos.Block.Types (Blund, Undo)
import           Pos.Core (Address (..), EpochIndex, HeaderHash, TxAux (..), difficultyL,
                           getChainDifficulty, makeRedeemAddress, siSlot)
import           Pos.Core.Block (Block, MainBlock, mainBlockSlot)
import           Pos.Core.Txp (Tx (..), TxId, taTx)
import           Pos.Txp (MonadTxpMem, getLocalTxs, topsortTxs, verifyTx, withTxpLocalData)
import           Pos.Txp.DB.Utxo (getTxOut)
import           Pos.Util (divRoundUp, maybeThrow)
import           Pos.Web (serveImpl)

import           Pos.BlockchainImporter.Aeson.ClientTypes ()
import           Pos.BlockchainImporter.BlockchainImporterMode (BlockchainImporterMode)
import           Pos.BlockchainImporter.DB (Page, defaultPageSize, getTxExtra)
import           Pos.BlockchainImporter.ExtraContext (HasBlockchainImporterCSLInterface (..))
import           Pos.BlockchainImporter.Web.Api (BlockchainImporterApi,
                                                 BlockchainImporterApiRecord (..),
                                                 blockchainImporterApi)
import           Pos.BlockchainImporter.Web.ClientTypes (CAddress (..), CBlockEntry (..),
                                                         CEncodedSTx (..), TxInternal (..),
                                                         decodeSTx, fromCAddress, getSlotIndex,
                                                         toBlockEntry)
import           Pos.BlockchainImporter.Web.Error (BlockchainImporterError (..))



----------------------------------------------------------------
-- Top level functionality
----------------------------------------------------------------

type MainBlund = (MainBlock, Undo)

blockchainImporterServeImpl
    :: BlockchainImporterMode ctx m
    => m Application
    -> Word16
    -> m ()
blockchainImporterServeImpl app port = serveImpl loggingApp "*" port Nothing Nothing Nothing
  where
    loggingApp = logStdoutDev <$> app

blockchainImporterApp :: BlockchainImporterMode ctx m => m (Server BlockchainImporterApi) -> m Application
blockchainImporterApp serv = serve blockchainImporterApi <$> serv

----------------------------------------------------------------
-- Handlers
----------------------------------------------------------------

blockchainImporterHandlers
    :: forall ctx m. BlockchainImporterMode ctx m
    => Diffusion m -> ServerT BlockchainImporterApi m
blockchainImporterHandlers _diffusion =
    toServant (BlockchainImporterApiRecord
        { _blockCount         = getBlocksTotal
        , _sendSignedTx       = sendSignedTx(_diffusion)
        }
        :: BlockchainImporterApiRecord (AsServerT m))

----------------------------------------------------------------
-- API Functions
----------------------------------------------------------------

-- | Get the total number of blocks/slots currently available.
-- Total number of main blocks   = difficulty of the topmost (tip) header.
-- Total number of anchor blocks = current epoch + 1
getBlocksTotal
    :: BlockchainImporterMode ctx m
    => m Integer
getBlocksTotal = do
    -- Get the tip block.
    tipBlock <- getTipBlockCSLI
    pure $ getBlockDifficulty tipBlock


-- | Get last blocks with a page parameter. This enables easier paging on the
-- client side and should enable a simple and thin client logic.
-- Currently the pages are in chronological order.
getBlocksPage
    :: BlockchainImporterMode ctx m
    => Maybe Word -- ^ Page number
    -> Maybe Word -- ^ Page size
    -> m (Integer, [CBlockEntry])
getBlocksPage mPageNumber mPageSize = do

    let pageSize = toPageSize mPageSize

    -- Get total pages from the blocks.
    totalPages <- getBlocksPagesTotal mPageSize

    -- Initially set on the last page number if page number not defined.
    let pageNumber = fromMaybe totalPages $ toInteger <$> mPageNumber

    -- Make sure the parameters are valid.
    when (pageNumber <= 0) $
        throwM $ Internal "Number of pages must be greater than 0."

    when (pageNumber > totalPages) $
        throwM $ Internal "Number of pages exceeds total pages number."

    -- TODO: Fix in the future.
    when (pageSize /= fromIntegral defaultPageSize) $
        throwM $ Internal "We currently support only page size of 10."

    when (pageSize > 1000) $
        throwM $ Internal "The upper bound for pageSize is 1000."

    -- Get pages from the database
    -- TODO: Fix this Int / Integer thing once we merge repositories
    pageBlocksHH    <- getPageHHsOrThrow $ fromIntegral pageNumber
    blunds          <- forM pageBlocksHH getBlundOrThrow
    cBlocksEntry    <- forM (blundToMainBlockUndo blunds) toBlockEntry

    -- Return total pages and the blocks. We start from page 1.
    pure (totalPages, reverse cBlocksEntry)
  where
    blundToMainBlockUndo :: [Blund] -> [(MainBlock, Undo)]
    blundToMainBlockUndo blund = [(mainBlock, undo) | (Right mainBlock, undo) <- blund]

    -- Either get the @HeaderHash@es from the @Page@ or throw an exception.
    getPageHHsOrThrow
        :: BlockchainImporterMode ctx m
        => Int
        -> m [HeaderHash]
    getPageHHsOrThrow pageNumber =
        -- Then let's fetch blocks for a specific page from it and raise exception if not
        -- found.
        getPageBlocksCSLI pageNumber >>= maybeThrow (Internal errMsg)
      where
        errMsg :: Text
        errMsg = sformat ("No blocks on page "%build%" found!") pageNumber

-- | Get total pages from blocks. Calculated from
-- pageSize we pass to it.
getBlocksPagesTotal
    :: BlockchainImporterMode ctx m
    => Maybe Word
    -> m Integer
getBlocksPagesTotal mPageSize = do

    let pageSize = toPageSize mPageSize

    -- Get total blocks in the blockchain. Get the blocks total using this mode.
    blocksTotal <- toInteger <$> getBlocksTotal

    -- Make sure the parameters are valid.
    when (blocksTotal < 1) $
        throwM $ Internal "There are currently no block to display."

    when (pageSize < 1) $
        throwM $ Internal "Page size must be greater than 1 if you want to display blocks."

    -- We start from page 1.
    let pagesTotal = roundToBlockPage blocksTotal

    pure pagesTotal


-- | Get the last page from the blockchain. We use the default 10
-- for the page size since this is called from __blockchainImporter only__.
getBlocksLastPage
    :: BlockchainImporterMode ctx m
    => m (Integer, [CBlockEntry])
getBlocksLastPage = getBlocksPage Nothing (Just defaultPageSizeWord)


-- | Search the blocks by epoch and slot.
getEpochSlot
    :: BlockchainImporterMode ctx m
    => EpochIndex
    -> Word16
    -> m [CBlockEntry]
getEpochSlot epochIndex slotIndex = do

    -- The slots start from 0 so we need to modify the calculation of the index.
    let page = fromIntegral $ (slotIndex `div` 10) + 1

    -- Get pages from the database
    -- TODO: Fix this Int / Integer thing once we merge repositories
    epochBlocksHH   <- getPageHHsOrThrow epochIndex page
    blunds          <- forM epochBlocksHH getBlundOrThrow
    cBlocksEntry    <- forM (getEpochSlots slotIndex (blundToMainBlockUndo blunds)) toBlockEntry

    pure cBlocksEntry
  where
    blundToMainBlockUndo :: [Blund] -> [(MainBlock, Undo)]
    blundToMainBlockUndo blund = [(mainBlock, undo) | (Right mainBlock, undo) <- blund]
    -- Get epoch slot block that's being searched or return all epochs if
    -- the slot is @Nothing@.
    getEpochSlots
        :: Word16
        -> [MainBlund]
        -> [MainBlund]
    getEpochSlots slotIndex' blunds = filter filterBlundsBySlotIndex blunds
      where
        getBlundSlotIndex
            :: MainBlund
            -> Word16
        getBlundSlotIndex blund = getSlotIndex $ siSlot $ fst blund ^. mainBlockSlot

        filterBlundsBySlotIndex
            :: MainBlund
            -> Bool
        filterBlundsBySlotIndex blund = getBlundSlotIndex blund == slotIndex'

    -- Either get the @HeaderHash@es from the @Epoch@ or throw an exception.
    getPageHHsOrThrow
        :: (HasBlockchainImporterCSLInterface m, MonadThrow m)
        => EpochIndex
        -> Int
        -> m [HeaderHash]
    getPageHHsOrThrow epoch page =
        getEpochBlocksCSLI epoch page >>= maybeThrow (Internal errMsg)
      where
        errMsg :: Text
        errMsg = sformat ("No blocks on epoch "%build%" page "%build%" found!") epoch page

-- | Search the blocks by epoch and epoch page number.
getEpochPage
    :: BlockchainImporterMode ctx m
    => EpochIndex
    -> Maybe Int
    -> m (Int, [CBlockEntry])
getEpochPage epochIndex mPage = do

    -- Get the page if it exists, return first page otherwise.
    let page = fromMaybe 1 mPage

    -- We want to fetch as many pages as we have in this @Epoch@.
    epochPagesNumber <- getEpochPagesOrThrow epochIndex

    -- Get pages from the database
    -- TODO: Fix this Int / Integer thing once we merge repositories
    epochBlocksHH       <- getPageHHsOrThrow epochIndex page
    blunds              <- forM epochBlocksHH getBlundOrThrow

    let sortedBlunds     = sortBlocksByEpochSlots blunds
    let sortedMainBlocks = blundToMainBlockUndo sortedBlunds

    cBlocksEntry        <- forM sortedMainBlocks toBlockEntry

    pure (epochPagesNumber, cBlocksEntry)
  where
    blundToMainBlockUndo :: [Blund] -> [(MainBlock, Undo)]
    blundToMainBlockUndo blund = [(mainBlock, undo) | (Right mainBlock, undo) <- blund]

    -- Either get the @HeaderHash@es from the @Epoch@ or throw an exception.
    getPageHHsOrThrow
        :: (HasBlockchainImporterCSLInterface m, MonadThrow m)
        => EpochIndex
        -> Int
        -> m [HeaderHash]
    getPageHHsOrThrow epoch page' =
        getEpochBlocksCSLI epoch page' >>= maybeThrow (Internal errMsg)
      where
        errMsg :: Text
        errMsg = sformat ("No blocks on epoch "%build%" page "%build%" found!") epoch page'

    -- | Sorting.
    sortBlocksByEpochSlots
        :: [(Block, Undo)]
        -> [(Block, Undo)]
    sortBlocksByEpochSlots blocks = sortOn (Down . getBlockIndex . fst) blocks
      where
        -- | Get the block index number. We start with the the index 1 for the
        -- genesis block and add 1 for the main blocks since they start with 1
        -- as well.
        getBlockIndex :: Block -> Int
        getBlockIndex (Left _)      = 1
        getBlockIndex (Right block) =
            fromIntegral $ (+1) $ getSlotIndex $ siSlot $ block ^. mainBlockSlot


sendSignedTx
     :: BlockchainImporterMode ctx m
     => Diffusion m
     -> CEncodedSTx
     -> m ()
sendSignedTx Diffusion{..} encodedSTx =
  exceptT' (hoistEither $ decodeSTx encodedSTx) (const $ throwM eInvalidEnc) $ \txAux -> do
    let txHash = hash $ taTx txAux
    -- FIXME: We are using only the confirmed UTxO, we should also take into account the pending txs
    exceptT' (verifyTx getTxOut False txAux) (throwM . eInvalidTx txHash) $ \_ -> do
      -- This is done for two reasons:
      -- 1. In order not to overflow relay.
      -- 2. To let other things (e. g. block processing) happen if
      -- `newPayment`s are done continuously.
      wasAccepted <- notFasterThan (6 :: Second) $ sendTx txAux
      void $ unless wasAccepted $ (throwM $ eNotAccepted txHash)
        where eInvalidEnc = Internal "Tx not broadcasted: invalid encoded tx"
              eInvalidTx txHash reason = Internal $
                  sformat ("Tx not broadcasted "%build%": "%build) txHash reason
              eNotAccepted txHash = Internal $
                  sformat  ("Tx broadcasted "%build%", not accepted by any peer") txHash
              exceptT' e f g = exceptT f g e


--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | A pure calculation of the page number.
-- Get total pages from the blocks. And we want the page
-- with the example, the page size 10,
-- to start with 10 + 1 == 11, not with 10 since with
-- 10 we'll have an empty page.
-- Could also be `((blocksTotal - 1) `div` pageSizeInt) + 1`.
roundToBlockPage :: Integer -> Integer
roundToBlockPage blocksTotal = divRoundUp blocksTotal $ fromIntegral defaultPageSize

-- | A pure function that return the number of blocks.
getBlockDifficulty :: Block -> Integer
getBlockDifficulty tipBlock = fromIntegral $ getChainDifficulty $ tipBlock ^. difficultyL

defaultPageSizeWord :: Word
defaultPageSizeWord = fromIntegral defaultPageSize

toPageSize :: Maybe Word -> Integer
toPageSize = fromIntegral . fromMaybe defaultPageSizeWord

getMempoolTxs :: BlockchainImporterMode ctx m => m [TxInternal]
getMempoolTxs = do

    localTxs <- fmap reverse $ topsortTxsOrFail mkWhTx =<< tlocalTxs

    fmap catMaybes . forM localTxs $ \(id, txAux) -> do
        mextra <- getTxExtra id
        forM mextra $ \extra -> pure $ TxInternal extra (taTx txAux)
  where
    tlocalTxs :: (MonadIO m, MonadTxpMem ext ctx m) => m [(TxId, TxAux)]
    tlocalTxs = withTxpLocalData getLocalTxs

    mkWhTx :: (TxId, TxAux) -> WithHash Tx
    mkWhTx (txid, txAux) = WithHash (taTx txAux) txid

topsortTxsOrFail :: (MonadThrow m, Eq a) => (a -> WithHash Tx) -> [a] -> m [a]
topsortTxsOrFail f =
    maybeThrow (Internal "Dependency loop in txs set") .
    topsortTxs f

-- Either get the block from the @HeaderHash@ or throw an exception.
getBlundOrThrow
    :: BlockchainImporterMode ctx m
    => HeaderHash
    -> m Blund
getBlundOrThrow headerHash =
    getBlundFromHHCSLI headerHash >>=
        maybeThrow (Internal "Blund with hash cannot be found!")


-- | Deserialize Cardano or RSCoin address and convert it to Cardano address.
-- Throw exception on failure.
cAddrToAddr :: MonadThrow m => CAddress -> m Address
cAddrToAddr cAddr@(CAddress rawAddrText) =
    -- Try decoding address as base64. If both decoders succeed,
    -- the output of the first one is returned
    let mDecodedBase64 =
            rightToMaybe (B64.decode rawAddrText) <|>
            rightToMaybe (B64.decodeUrl rawAddrText)

    in case mDecodedBase64 of
        Just addr -> do
            -- the decoded address can be both the RSCoin address and the Cardano address.
            -- > RSCoin address == 32 bytes
            -- > Cardano address >= 34 bytes
            if (BS.length addr == 32)
                then pure $ makeRedeemAddress $ redeemPkBuild addr
                else either badCardanoAddress pure (fromCAddress cAddr)
        Nothing ->
            -- cAddr is in Cardano address format or it's not valid
            either badCardanoAddress pure (fromCAddress cAddr)
  where

    badCardanoAddress = const $ throwM $ Internal "Invalid Cardano address!"


-- | Get @Page@ numbers from an @Epoch@ or throw an exception.
getEpochPagesOrThrow
    :: (HasBlockchainImporterCSLInterface m, MonadThrow m)
    => EpochIndex
    -> m Page
getEpochPagesOrThrow epochIndex =
    getEpochPagesCSLI epochIndex >>= maybeThrow (Internal "No epoch pages.")

----------------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------------

notFasterThan ::
       (Mockable Concurrently m, Mockable Delay m) => Second -> m a -> m a
notFasterThan time action = fst <$> concurrently action (delay time)
