-- | Types describing runtime errors related to BlockchainImporter

module Pos.BlockchainImporter.Web.Error
       ( BlockchainImporterError (..)
       ) where

import qualified Data.Text.Buildable
import           Formatting (bprint, stext, (%))
import           Universum

newtype BlockchainImporterError =
    -- | Some internal error.
    Internal Text
    deriving (Show, Generic)

instance Exception BlockchainImporterError

instance Buildable BlockchainImporterError where
    build (Internal msg) = bprint ("Internal blockchainImporter error ("%stext%")") msg
