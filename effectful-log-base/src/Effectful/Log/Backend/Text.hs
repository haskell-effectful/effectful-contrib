-- | A logger that produces in-memory 'Text' values. Mainly useful for
-- testing.
module Effectful.Log.Backend.Text
  ( withSimpleTextLogger
  ) where

import Data.Text (Text)
import qualified Log.Backend.Text as LogBase
import Effectful.Internal.Monad

import Effectful.Log.Logger

-- | Lifted 'LogBase.withSimpleTextLogger'.
withSimpleTextLogger :: (Logger -> Eff es a) -> Eff es (Text, a)
withSimpleTextLogger act = unsafeEff $ \es -> do
  LogBase.withSimpleTextLogger ((`unEff` es) . act)
