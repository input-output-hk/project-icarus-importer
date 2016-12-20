{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module Pos.Ssc.Class.Helpers
       (
         SscHelpersClass (..)
       , SscHelpersClassM (..)
       , sscCalculateSeed
       ) where

import           Data.Tagged           (Tagged)
import           Serokell.Util.Verify  (VerificationRes)
import           Universum

import           Pos.Crypto            (Threshold)
import           Pos.Ssc.Class.Storage (MonadSscGS, SscGlobalQueryM, sscRunGlobalQuery)
import           Pos.Ssc.Class.Types   (Ssc (..))
import           Pos.Types.Types       (MainBlockHeader, SharedSeed)
import           Pos.Types.Types       (EpochIndex)

class Ssc ssc => SscHelpersClass ssc where
    sscVerifyPayload :: Tagged ssc (MainBlockHeader ssc -> SscPayload ssc -> VerificationRes)

class Ssc ssc => SscHelpersClassM ssc where
    sscVerifyPayloadM :: Tagged ssc (MainBlockHeader ssc -> SscPayload ssc -> VerificationRes)

    sscCalculateSeedQ :: EpochIndex -> Threshold -> SscGlobalQueryM ssc (Either (SscSeedError ssc) SharedSeed)

sscCalculateSeed
    :: forall ssc m.
       (MonadSscGS ssc m, SscHelpersClassM ssc)
    => EpochIndex -> Threshold -> m (Either (SscSeedError ssc) SharedSeed)
sscCalculateSeed e = sscRunGlobalQuery . sscCalculateSeedQ @ssc e
