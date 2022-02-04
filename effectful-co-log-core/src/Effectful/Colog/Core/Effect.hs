module Effectful.Colog.Core.Effect
  ( Log,
    LogActionEff,
    runLog,
    logMsg,
    logMsgs,
    withLog,
  )
where

import Colog.Core (LogAction (..))
import qualified Colog.Core as Colog
import Data.Foldable (traverse_)
import Effectful.Dispatch.Static
  ( StaticRep,
    evalStaticRep,
    forkEnv,
    getEnv,
    localStaticRep,
    unEff,
    unsafeEff,
    unsafeEff_,
  )
import Effectful.Monad
  ( Dispatch (Static),
    DispatchOf,
    Eff,
    Effect,
    type (:>),
  )

-- | An effect for composable, contravariant and comonadic logging using the @co-log@ library.
data Log msg :: Effect

type instance DispatchOf (Log msg) = 'Static

newtype instance StaticRep (Log msg) = Log (LogAction IO msg)

-- | 'LogAction' that works directly with the 'Eff' monad.
type LogActionEff es msg = LogAction (Eff es) msg

-- | Run a 'Log' effect.
--
-- This function is the effectful version of 'Colog.usingLoggerT'
runLog :: forall msg a es. LogActionEff es msg -> Eff (Log msg ': es) a -> Eff es a
runLog action m = do
  env <- unsafeEff forkEnv
  evalStaticRep (Log $ Colog.hoistLogAction (`unEff` env) action) m

-- | Perform logging action with given @msg@.
--
-- The effectful version of 'Colog.logMsg'.
logMsg :: forall msg es. Log msg :> es => msg -> Eff es ()
logMsg msg = unsafeEff $ \env -> do
  Log (LogAction action) <- getEnv env
  action msg

-- | The effectful version of 'Colog.logMsgs'.
logMsgs :: forall msg f es. (Foldable f, Log msg :> es) => f msg -> Eff es ()
logMsgs = traverse_ logMsg

-- | The effectful version of 'Colog.withLog'.
withLog :: forall msg a es. Log msg :> es => (LogActionEff es msg -> LogActionEff es msg) -> Eff es a -> Eff es a
withLog f m = do
  env <- unsafeEff forkEnv
  let f' = Colog.hoistLogAction (`unEff` env) . f . Colog.hoistLogAction unsafeEff_
  localStaticRep (\(Log action) -> Log (f' action)) m