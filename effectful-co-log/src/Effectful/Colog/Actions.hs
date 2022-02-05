-- | Effectfull functions from "Colog.Actions".
module Effectful.Colog.Actions
  ( -- * 'ByteString' actions
    logByteStringStdout,
    logByteStringStderr,
    logByteStringHandle,
    withLogByteStringFile,

    -- * 'Text' actions
    logTextStdout,
    logTextStderr,
    logTextHandle,
    withLogTextFile,

    -- * 'Message' actions
    simpleMessageAction,
    richMessageAction,
  )
where

import qualified Colog
import Colog.Message (Message)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Effectful (Eff, IOE, type (:>))
import Effectful.Colog.Core (LogActionEff)
import Effectful.Dispatch.Static (unsafeUnliftIO)
import System.IO (Handle)

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
withLogByteStringFile :: IOE :> es => FilePath -> (LogActionEff es ByteString -> Eff es r) -> Eff es r
withLogByteStringFile path action = unsafeUnliftIO $ \runInIO ->
  Colog.withLogByteStringFile path (runInIO . action)

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
withLogTextFile :: IOE :> es => FilePath -> (LogActionEff es Text -> Eff es r) -> Eff es r
withLogTextFile path action = unsafeUnliftIO $ \runInIO ->
  Colog.withLogTextFile path (runInIO . action)

-- | The effectful version of 'Colog.simpleMessageAction'.
simpleMessageAction :: IOE :> es => LogActionEff es Message
simpleMessageAction = Colog.simpleMessageAction

-- | The effectful version of 'Colog.richMessageAction'.
richMessageAction :: IOE :> es => LogActionEff es Message
richMessageAction = Colog.richMessageAction
