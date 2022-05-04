-- | Effectfull functions from "Colog.Pure".
module Effectful.Colog.Pure
  ( logMessagePure,
    runPureLogEff,
    execPureLogEff,
  )
where

import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Effectful (Eff, type (:>))
import Effectful.Colog.Core
  ( Log,
    LogAction (..),
    LogActionEff,
    runLog,
  )
import qualified Effectful.State.Static.Shared as Shared

-- | LogAction that prints msg by appending it to the end of the sequence.
--
-- The effectful version of 'Colog.logMessagePure'.
logMessagePure :: forall msg es. Shared.State (Seq msg) :> es => LogActionEff es msg
logMessagePure = LogAction $ \msg -> Shared.modify (|> msg)

-- | Run the 'Log' effect with the 'logMessagePure' action and return the final value along with the sequence of messages.
runPureLogEff :: forall msg a es. Eff (Log msg ': Shared.State (Seq msg) ': es) a -> Eff es (a, Seq msg)
runPureLogEff = Shared.runState Seq.empty . runLog (logMessagePure @msg)

-- | Run the 'Log' effect with the 'logMessagePure' action and return the sequence of messages, discarding the final value.
execPureLogEff :: forall msg a es. Eff (Log msg ': Shared.State (Seq msg) ': es) a -> Eff es (Seq msg)
execPureLogEff = Shared.execState Seq.empty . runLog (logMessagePure @msg)
