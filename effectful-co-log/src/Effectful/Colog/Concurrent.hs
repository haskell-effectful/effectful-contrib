-- | Effectful functions from "Colog.Concurrent".
module Effectful.Colog.Concurrent
  ( withBackgroundLogger,
    Colog.defCapacity,
    killBackgroundLogger,
    forkBackgroundLogger,
    convertToLogAction,
    mkBackgroundThread,
    runInBackgroundThread,
  )
where

import qualified Colog
import Colog.Concurrent.Internal (BackgroundWorker (..), Capacity)
import Effectful
  ( Eff,
    IOE,
    Limit(..),
    Persistence(..),
    UnliftStrategy(..),
    type (:>),
    unliftStrategy,
    withEffToIO,
    withUnliftStrategy,
  )
import Effectful.Colog.Core (LogAction (..), LogActionEff)
import Effectful.Concurrent.STM (Concurrent, atomically)
import Effectful.Dispatch.Static
  ( unsafeEff_,
  )

-- | The effectful version of 'Colog.withBackgroundLogger'.
withBackgroundLogger ::
  forall msg a es es'.
  (Concurrent :> es, IOE :> es) =>
  Capacity ->
  LogActionEff es msg ->
  (LogActionEff es' msg -> Eff es a) ->
  Eff es a
withBackgroundLogger capacity logger action = do
  strategy <- unliftStrategy
  let action' = withUnliftStrategy strategy . action
  withUnliftStrategy (ConcUnlift Persistent Unlimited) $
    withEffToIO $ \runInIO ->
      Colog.withBackgroundLogger
        capacity
        (Colog.hoistLogAction runInIO logger)
        (runInIO . action' . Colog.hoistLogAction unsafeEff_)

-- | The effectful version of 'Colog.killBackgroundLogger'.
killBackgroundLogger :: Concurrent :> es => BackgroundWorker msg -> Eff es ()
killBackgroundLogger = unsafeEff_ . Colog.killBackgroundLogger

-- | The effectful version of 'Colog.forkBackgroundLogger'.
forkBackgroundLogger ::
  (Concurrent :> es, IOE :> es) =>
  Capacity ->
  LogActionEff es msg ->
  Eff es (BackgroundWorker msg)
forkBackgroundLogger capacity logger =
  withUnliftStrategy (ConcUnlift Persistent Unlimited) $
    withEffToIO $ \runInIO ->
      Colog.forkBackgroundLogger
        capacity
        (Colog.hoistLogAction runInIO logger)

-- | The effectful version of 'Colog.convertToLogAction'.
convertToLogAction :: Concurrent :> es => BackgroundWorker msg -> LogActionEff es msg
convertToLogAction worker = LogAction $ \msg ->
  atomically $ backgroundWorkerWrite worker msg

-- | The effectful version of 'Colog.mkBackgroundThread'.
mkBackgroundThread ::
  (Concurrent :> es, IOE :> es) =>
  Capacity ->
  Eff es (BackgroundWorker (Eff es ()))
mkBackgroundThread capacity =
  withUnliftStrategy (ConcUnlift Persistent Unlimited) $
    withEffToIO $ \runInIO ->
      cmapBackgroundWorker runInIO
        <$> Colog.mkBackgroundThread capacity

-- | The effectful version of 'Colog.runInBackgroundThread'.
runInBackgroundThread ::
  Concurrent :> es =>
  BackgroundWorker (Eff es ()) ->
  LogActionEff es msg ->
  LogActionEff es msg
runInBackgroundThread worker logger = LogAction $ \msg ->
  atomically $ backgroundWorkerWrite worker $ unLogAction logger msg

----------------------------------------
-- Helper functions
----------------------------------------

cmapBackgroundWorker :: (a -> b) -> BackgroundWorker b -> BackgroundWorker a
cmapBackgroundWorker f worker = worker
  { backgroundWorkerWrite = backgroundWorkerWrite worker . f
  }
