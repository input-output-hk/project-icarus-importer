module Pos.PostgresConsistency.Utils where

import           Universum

import           Pos.Core (HeaderHash)
import           Pos.Crypto (decodeHash)

decodeBlkHash :: Text -> Maybe HeaderHash
decodeBlkHash = rightToMaybe . decodeHash

maybeT :: Monad m => m (Maybe a) -> m b -> (a -> m b) -> m b
maybeT maybeM nothingValue justFn = do
  maybeA <- maybeM
  case maybeA of
    Nothing -> nothingValue
    Just a  -> justFn a
