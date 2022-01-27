module Effectful.Colog
  ( -- * Log effect
    Log,
    LogAction (..),
    LogActionEff,
    runLog,

    -- * Effectful functions of 'Colog.Monad'
    logMsg,
    logMsgs,
    withLog,

    -- * Messages

    -- ** Simple message
    SimpleMsg (..),
    logText,
    Colog.fmtSimpleMessage,
    Colog.formatWith,

    -- ** Core message

    -- *** Types
    Msg (..),
    Severity (..),
    Message,

    -- *** Logging
    log,
    logDebug,
    logInfo,
    logWarning,
    logError,
    logException,

    -- *** Formatting
    Colog.fmtMessage,
    Colog.showSeverity,
    Colog.showSourceLoc,

    -- ** Externally extensible message type
    Colog.FieldType,
    Colog.MessageField (..),
    Colog.unMessageField,
    Colog.extractField,
    Colog.FieldMap,
    defaultFieldMap,
    Colog.RichMessage,
    Colog.RichMsg,
    fmtRichMessageDefault,
    fmtSimpleRichMessageDefault,
    upgradeMessageAction,

    -- * Log actions

    -- ** ByteString actions
    logByteStringStdout,
    logByteStringStderr,
    logByteStringHandle,
    withLogByteStringFile,

    -- ** Text actions
    logTextStdout,
    logTextStderr,
    logTextHandle,
    withLogTextFile,

    -- ** Message actions
    simpleMessageAction,
    richMessageAction,

    -- ** Pure actions
    logMessagePure,
  )
where

import Colog
  ( LogAction (..),
    Message,
    Msg (..),
    Severity (..),
    SimpleMsg (..),
  )
import qualified Colog
import Control.Exception (Exception (displayException))
import Data.ByteString (ByteString)
import Data.Foldable (traverse_)
import Data.Sequence (Seq, (|>))
import Data.Text (Text)
import qualified Data.Text as Text
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
    IOE,
    type (:>),
  )
import Effectful.State.Shared (State, modify)
import GHC.IO.Handle (Handle)
import GHC.Stack (callStack, withFrozenCallStack)
import Prelude hiding (log)

-- | An effect for composable, contravariant and comonadic logging using the @co-log@ library.
data Log msg :: Effect

type instance DispatchOf (Log msg) = 'Static

newtype instance StaticRep (Log msg) = Log (LogAction IO msg)

-- | 'LogAction' that works directly with the 'Eff' monad.
type LogActionEff es msg = LogAction (Eff es) msg

-- | Run a 'Log' effect.
--
-- This function is the effectful version of 'Colog.usingLoggerT'
runLog :: LogActionEff es msg -> Eff (Log msg ': es) a -> Eff es a
runLog action m = do
  env <- unsafeEff forkEnv
  evalStaticRep (Log $ Colog.hoistLogAction (`unEff` env) action) m

-- | Perform logging action with given @msg@.
--
-- The effectful version of 'Colog.logMsg'.
logMsg :: Log msg :> es => msg -> Eff es ()
logMsg msg = unsafeEff $ \env -> do
  Log (LogAction action) <- getEnv env
  action msg

-- | The effectful version of 'Colog.logMsgs'.
logMsgs :: (Foldable f, Log msg :> es) => f msg -> Eff es ()
logMsgs = traverse_ logMsg

-- | The effectful version of 'Colog.withLog'.
withLog :: Log msg :> es => (LogActionEff es msg -> LogActionEff es msg) -> Eff es a -> Eff es a
withLog f m = do
  env <- unsafeEff forkEnv
  let f' = Colog.hoistLogAction (`unEff` env) . f . Colog.hoistLogAction unsafeEff_
  localStaticRep (\(Log action) -> Log (f' action)) m

-- | The effectful version of 'Colog.logText'.
logText :: Log SimpleMsg :> es => Text -> Eff es ()
logText msg = withFrozenCallStack (logMsg SimpleMsg {simpleMsgStack = callStack, simpleMsgText = msg})

-- | The effectful version of 'Colog.log'.
log :: Log (Msg sev) :> es => sev -> Text -> Eff es ()
log sev msg = withFrozenCallStack (logMsg Msg {msgStack = callStack, msgSeverity = sev, msgText = msg})

-- | The effectful version of 'Colog.logDebug'.
logDebug :: Log Message :> es => Text -> Eff es ()
logDebug = withFrozenCallStack (log Debug)

-- | The effectful version of 'Colog.logInfo'.
logInfo :: Log Message :> es => Text -> Eff es ()
logInfo = withFrozenCallStack (log Info)

-- | The effectful version of 'Colog.logWarning'.
logWarning :: Log Message :> es => Text -> Eff es ()
logWarning = withFrozenCallStack (log Warning)

-- | The effectful version of 'Colog.logError'.
logError :: Log Message :> es => Text -> Eff es ()
logError = withFrozenCallStack (log Error)

-- | The effectful version of 'Colog.logException'.
logException :: (Log Message :> es, Exception e) => e -> Eff es ()
logException = withFrozenCallStack (logError . Text.pack . displayException)

-- | The effectful version of 'Colog.defaultFieldMap'
defaultFieldMap :: IOE :> es => Colog.FieldMap (Eff es)
defaultFieldMap = Colog.defaultFieldMap

-- | The effectful version of 'Colog.fmtRichMessageDefault'
fmtRichMessageDefault :: IOE :> es => Colog.RichMessage (Eff es) -> Eff es Text
fmtRichMessageDefault = Colog.fmtRichMessageDefault

-- | The effectful version of 'Colog.fmtSimpleRichMessageDefault'
fmtSimpleRichMessageDefault :: IOE :> es => Colog.RichMsg (Eff es) SimpleMsg -> Eff es Text
fmtSimpleRichMessageDefault = Colog.fmtSimpleRichMessageDefault

-- | The effectful version of 'Colog.upgradeMessageAction'
upgradeMessageAction :: IOE :> es => Colog.FieldMap (Eff es) -> LogActionEff es (Colog.RichMsg (Eff es) msg) -> LogActionEff es msg
upgradeMessageAction = Colog.upgradeMessageAction

-- | The effectful version of 'Colog.logByteStringStdout'.
logByteStringStdout :: IOE :> es => LogActionEff es ByteString
logByteStringStdout = Colog.logByteStringStdout

-- | The effectful version of 'Colog.logByteStringStderr'.
logByteStringStderr :: IOE :> es => LogActionEff es ByteString
logByteStringStderr = Colog.logByteStringStderr

-- | The effectful version of 'Colog.logByteStringHandle'.
logByteStringHandle :: IOE :> es => Handle -> LogActionEff es ByteString
logByteStringHandle = Colog.logByteStringHandle

-- | The effectful version of 'Colog.withLogByteStringFile'.
withLogByteStringFile :: IOE :> es => FilePath -> (LogActionEff es ByteString -> IO r) -> Eff es r
withLogByteStringFile path action = unsafeEff_ $ Colog.withLogByteStringFile path action

-- | The effectful version of 'Colog.logTextStdout'.
logTextStdout :: IOE :> es => LogActionEff es Text
logTextStdout = Colog.logTextStdout

-- | The effectful version of 'Colog.logTextStderr'.
logTextStderr :: IOE :> es => LogActionEff es Text
logTextStderr = Colog.logTextStderr

-- | The effectful version of 'Colog.logTextHandle'.
logTextHandle :: IOE :> es => Handle -> LogActionEff es Text
logTextHandle = Colog.logTextHandle

-- | The effectful version of 'Colog.withLogTextFile'.
withLogTextFile :: IOE :> es => FilePath -> (LogActionEff es Text -> IO r) -> Eff es r
withLogTextFile path action = unsafeEff_ $ Colog.withLogTextFile path action

-- | The effectful version of 'Colog.simpleMessageAction'.
simpleMessageAction :: IOE :> es => LogActionEff es Message
simpleMessageAction = Colog.simpleMessageAction

-- | The effectful version of 'Colog.richMessageAction'.
richMessageAction :: IOE :> es => LogActionEff es Message
richMessageAction = Colog.richMessageAction

-- | LogAction that prints msg by appending it to the end of the sequence.
--
-- The effectful version of 'Colog.logMessagePure'.
logMessagePure :: State (Seq msg) :> es => LogActionEff es msg
logMessagePure = LogAction $ \msg -> modify (|> msg)
