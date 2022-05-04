-- | Effectful functions from "Colog.Core.IO".
module Effectful.Colog.Core.IO
  ( -- * 'String' actions
    logStringStdout,
    logStringStderr,
    logStringHandle,
    withLogStringFile,

    -- * 'Show' actions
    logPrint,
    logPrintStderr,
    logPrintHandle,

    -- * Various combinators
    withLogPrintFile,
    liftLogIO,
  )
where

import qualified Colog.Core as Colog
import Colog.Core.Action (LogAction)
import Effectful (Eff, IOE, type (:>))
import Effectful.Colog.Core.Effect (LogActionEff)
import Effectful.Dispatch.Static.Unsafe (reallyUnsafeUnliftIO)
import System.IO (Handle)

-- | The effectul version of 'Colog.logStringStdout'
logStringStdout :: IOE :> es => LogActionEff es String
logStringStdout = Colog.logStringStdout

-- | The effectul version of 'Colog.logStringStderr'
logStringStderr :: IOE :> es => LogActionEff es String
logStringStderr = Colog.logStringStderr

-- | The effectul version of 'Colog.logStringHandle'
logStringHandle :: IOE :> es => Handle -> LogActionEff es String
logStringHandle = Colog.logStringHandle

-- | The effectul version of 'Colog.withLogStringFile'
withLogStringFile :: IOE :> es => FilePath -> (LogActionEff es String -> Eff es r) -> Eff es r
withLogStringFile path action = reallyUnsafeUnliftIO $ \runInIO ->
  Colog.withLogStringFile path (runInIO . action)

-- | The effectul version of 'Colog.logPrint'
logPrint :: forall msg es. (Show msg, IOE :> es) => LogActionEff es msg
logPrint = Colog.logPrint

-- | The effectul version of 'Colog.logPrintStderr'
logPrintStderr :: forall msg es. (Show msg, IOE :> es) => LogActionEff es msg
logPrintStderr = Colog.logPrintStderr

-- | The effectul version of 'Colog.logPrintHandle'
logPrintHandle :: forall msg es. (Show msg, IOE :> es) => Handle -> LogActionEff es msg
logPrintHandle = Colog.logPrintHandle

-- | The effectul version of 'Colog.withLogPrintFile'
withLogPrintFile :: forall msg r es. (Show msg, IOE :> es) => FilePath -> (LogActionEff es msg -> Eff es r) -> Eff es r
withLogPrintFile path action = reallyUnsafeUnliftIO $ \runInIO ->
  Colog.withLogPrintFile path (runInIO . action)

-- | The effectul version of 'Colog.liftLogIO'
liftLogIO :: forall msg es. IOE :> es => LogAction IO msg -> LogActionEff es msg
liftLogIO = Colog.liftLogIO
