module Effectful.Process.Typed
  ( -- * Process effect
    TypedProcess
  , runTypedProcess

  -- * Launch a process
  , startProcess
  , stopProcess
  , withProcessWait
  , withProcessWait_
  , withProcessTerm
  , withProcessTerm_
  , readProcess
  , readProcess_
  , runProcess
  , runProcess_
  , runProcessStdout
  , runProcessStdout_
  , runProcessStderr
  , runProcessStderr_
  , runProcessInterleaved
  , runProcessInterleaved_

  -- * Process exit code
  , waitExitCode
  , getExitCode
  , checkExitCode
  ) where

import Data.ByteString.Lazy (ByteString)
import System.Exit (ExitCode)
import qualified System.Process.Typed as PT

import Effectful.Internal.Effect
import Effectful.Internal.Monad

-- | An effect for running child processes using the @typed-process@ library.
data TypedProcess :: Effect where
  TypedProcess :: TypedProcess m r

runTypedProcess :: IOE :> es => Eff (TypedProcess : es) a -> Eff es a
runTypedProcess = evalEffect (IdE TypedProcess)

----------------------------------------
-- Launch a process

-- | Lifted 'PT.startProcess'.
startProcess :: TypedProcess :> es
             => PT.ProcessConfig stdin stdout stderr
             -> Eff es (PT.Process stdin stdout stderr)
startProcess = unsafeEff_ . PT.startProcess

-- | Lifted 'PT.stopProcess'.
stopProcess :: TypedProcess :> es => PT.Process stdin stdout stderr -> Eff es ()
stopProcess = unsafeEff_ . PT.stopProcess

-- | Lifted 'PT.withProcessWait'.
withProcessWait :: TypedProcess :> es
                => PT.ProcessConfig stdin stdout stderr
                -> (PT.Process stdin stdout stderr -> Eff es a)
                -> Eff es a
withProcessWait = liftWithProcess PT.withProcessWait

-- | Lifted 'PT.withProcessWait_'.
withProcessWait_ :: TypedProcess :> es
                 => PT.ProcessConfig stdin stdout stderr
                 -> (PT.Process stdin stdout stderr -> Eff es a)
                 -> Eff es a
withProcessWait_ = liftWithProcess PT.withProcessWait_

-- | Lifted 'PT.withProcessTerm'.
withProcessTerm :: TypedProcess :> es
                => PT.ProcessConfig stdin stdout stderr
                -> (PT.Process stdin stdout stderr -> Eff es a)
                -> Eff es a
withProcessTerm = liftWithProcess PT.withProcessTerm

-- | Lifted 'PT.withProcessTerm_'.
withProcessTerm_ :: TypedProcess :> es
                 => PT.ProcessConfig stdin stdout stderr
                 -> (PT.Process stdin stdout stderr -> Eff es a)
                 -> Eff es a
withProcessTerm_ = liftWithProcess PT.withProcessTerm_

-- | Lifted 'PT.readProcess'.
readProcess :: TypedProcess :> es
            => PT.ProcessConfig stdin stdoutIgnored stderrIgnored
            -> Eff es (ExitCode, ByteString, ByteString)
readProcess = unsafeEff_ . PT.readProcess

-- | Lifted 'PT.readProcess_'.
readProcess_ :: TypedProcess :> es
             => PT.ProcessConfig stdin stdoutIgnored stderrIgnored
             -> Eff es (ByteString, ByteString)
readProcess_ = unsafeEff_ . PT.readProcess_

-- | Lifted 'PT.runProcess'.
runProcess :: TypedProcess :> es
           => PT.ProcessConfig stdin stdout stderr
           -> Eff es ExitCode
runProcess = unsafeEff_ . PT.runProcess

-- | Lifted 'PT.runProcess_'.
runProcess_ :: TypedProcess :> es
            => PT.ProcessConfig stdin stdout stderr
            -> Eff es ()
runProcess_ = unsafeEff_ . PT.runProcess_

-- | Lifted 'PT.runProcessStdout'.
runProcessStdout :: TypedProcess :> es
                 => PT.ProcessConfig stdin stdoutIgnored stderr
                 -> Eff es (ExitCode, ByteString)
runProcessStdout = unsafeEff_ . PT.readProcessStdout

-- | Lifted 'PT.runProcessStdout_'.
runProcessStdout_ :: TypedProcess :> es
                  => PT.ProcessConfig stdin stdoutIgnored stderr
                  -> Eff es ByteString
runProcessStdout_ = unsafeEff_ . PT.readProcessStdout_

-- | Lifted 'PT.runProcessStderr'.
runProcessStderr :: TypedProcess :> es
                 => PT.ProcessConfig stdin stdout stderrIgnored
                 -> Eff es (ExitCode, ByteString)
runProcessStderr = unsafeEff_ . PT.readProcessStderr

-- | Lifted 'PT.runProcessStderr_'.
runProcessStderr_ :: TypedProcess :> es
                  => PT.ProcessConfig stdin stdout stderrIgnored
                  -> Eff es ByteString
runProcessStderr_ = unsafeEff_ . PT.readProcessStderr_

-- | Lifted 'PT.runProcessInterleaved'.
runProcessInterleaved :: TypedProcess :> es
                      => PT.ProcessConfig stdin stdoutIgnored stderrIgnored
                      -> Eff es (ExitCode, ByteString)
runProcessInterleaved = unsafeEff_ . PT.readProcessInterleaved

-- | Lifted 'PT.runProcessInterleaved_'.
runProcessInterleaved_ :: TypedProcess :> es
                       => PT.ProcessConfig stdin stdoutIgnored stderrIgnored
                       -> Eff es ByteString
runProcessInterleaved_ = unsafeEff_ . PT.readProcessInterleaved_

----------------------------------------
-- Process exit code

-- | Lifted 'PT.waitExitCode'.
waitExitCode :: TypedProcess :> es
             => PT.Process stdin stdout stderr
             -> Eff es ExitCode
waitExitCode = unsafeEff_ . PT.waitExitCode

---- | Lifted 'PT.waitExitCodeSTM'.
--waitExitCodeSTM :: TypedProcess :> es
--                => PT.Process stdin stdout stderr
--                -> Eff es ExitCode
--waitExitCodeSTM = unsafeEff_ . PT.waitExitCode

-- | Lifted 'PT.getExitCode'.
getExitCode :: TypedProcess :> es
            => PT.Process stdin stdout stderr
            -> Eff es (Maybe ExitCode)
getExitCode = unsafeEff_ . PT.getExitCode

---- | Lifted 'PT.getExitCodeSTM'.
--getExitCodeSTM :: TypedProcess :> es
--               => PT.Process stdin stdout stderr
--               -> Eff es (Maybe ExitCode)
--getExitCodeSTM = unsafeEff_ . PT.getExitCodeSTM

-- | Lifted 'PT.checkExitCode'.
checkExitCode :: TypedProcess :> es
              => PT.Process stdin stdout stderr
              -> Eff es ()
checkExitCode = unsafeEff_ . PT.checkExitCode

---- | Lifted 'PT.checkExitCodeSTM'.
--checkExitCodeSTM :: TypedProcess :> es
--                 => PT.Process stdin stdout stderr
--                 -> Eff es ()
--checkExitCodeSTM = unsafeEff_ . PT.checkExitCodeSTM

----------------------------------------
-- Helpers

liftWithProcess :: TypedProcess :> es
                => (PT.ProcessConfig stdin stdout stderr -> (PT.Process stdin stdout stderr -> IO a) -> IO a)
                -> PT.ProcessConfig stdin stdout stderr
                -> (PT.Process stdin stdout stderr -> Eff es a)
                -> Eff es a
liftWithProcess k pc f = unsafeEff $ \es ->
  unsafeSeqUnliftEff es $ \runInIO ->
    k pc (runInIO . f)
