-- | Effectfull functions from "Colog.Message".
module Effectful.Colog.Message
  ( -- * Simple message type

    -- ** Type
    SimpleMsg (..),

    -- ** Logging
    logText,

    -- ** Formatting
    Colog.fmtSimpleMessage,
    Colog.formatWith,

    -- * Core messaging

    -- ** Types
    Msg (..),
    Message,

    -- ** Logging
    log,
    logDebug,
    logInfo,
    logWarning,
    logError,
    logException,

    -- ** Formatting
    Colog.fmtMessage,
    Colog.showSeverity,
    Colog.showSourceLoc,

    -- * Externally extensible message type

    -- ** Field of the dependent map
    Colog.FieldType,
    Colog.MessageField (..),
    Colog.unMessageField,
    Colog.extractField,

    -- ** Dependent map that allows to extend logging message
    Colog.FieldMap,
    defaultFieldMap,

    -- ** Extensible message
    Colog.RichMessage,
    Colog.RichMsg (..),
    fmtRichMessageDefault,
    fmtSimpleRichMessageDefault,
    fmtRichMessageCustomDefault,
    upgradeMessageAction,
  )
where

import Chronos (Time)
import qualified Colog
import Colog.Message (Message, Msg (..), SimpleMsg (..))
import Control.Concurrent (ThreadId)
import Control.Exception (Exception (displayException))
import Data.Text (Text)
import qualified Data.Text as Text
import Effectful (Eff, IOE, type (:>))
import Effectful.Colog.Core
import GHC.Stack (callStack, withFrozenCallStack)
import Prelude hiding (log)

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

-- | The effectful version of 'Colog.fmtRichMessageCustomDefault'
fmtRichMessageCustomDefault :: IOE :> es => Colog.RichMsg (Eff es) msg -> (Maybe ThreadId -> Maybe Time -> msg -> Text) -> Eff es Text
fmtRichMessageCustomDefault = Colog.fmtRichMessageCustomDefault

-- | The effectful version of 'Colog.upgradeMessageAction'
upgradeMessageAction :: Colog.FieldMap (Eff es) -> LogActionEff es (Colog.RichMsg (Eff es) msg) -> LogActionEff es msg
upgradeMessageAction = Colog.upgradeMessageAction
