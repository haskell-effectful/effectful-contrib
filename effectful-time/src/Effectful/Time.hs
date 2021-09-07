{-# LANGUAGE Strict #-}
{-|
  Module      : Effectful.Time
  Copyright   : © Hécate Moonlight, 2021
  License     : MIT
  Maintainer  : hecate@glitchbra.in
  Stability   : stable

  An effect wrapper around Data.Time for the Effectful ecosystem
-}
module Effectful.Time where

import Control.Monad.IO.Class
import Data.Kind
import Data.Time (UTCTime)
import Effectful.Handler
import Effectful.Monad
import qualified Data.Time as T

-- | An effect for getting the current time
data Time :: Effect where
  CurrentTime :: Time m UTCTime

-- | Retrieve the current time in your effect stack
getCurrentTime :: forall (es :: [Effect])
                . Time :> es => Eff es UTCTime
getCurrentTime = send CurrentTime

-- | The default IO handler
runCurrentTimeIO :: forall (es :: [Effect]) (a :: Type)
                  . IOE :> es => Eff (Time : es) a -> Eff es a
runCurrentTimeIO = interpret $ \_ CurrentTime -> liftIO T.getCurrentTime

-- | The pure handler, with a static value
runCurrentTimePure :: forall (es :: [Effect]) (a :: Type)
                    . UTCTime -> Eff (Time : es) a -> Eff es a
runCurrentTimePure time = interpret $ \_ CurrentTime -> pure time
