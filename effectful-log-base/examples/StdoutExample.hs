{-# LANGUAGE OverloadedStrings #-}

module StdoutExample (main) where

import Data.Text (Text)
import Effectful.Monad
import Log (logInfo)

import Effectful.Log
import Effectful.Log.Backend.StandardOutput

main :: IO ()
main = runEff $ do
  withSimpleStdOutLogger $ \logger -> do
    runLogging "main" logger $ do
      app

app :: Logging :> es => Eff es ()
app = do
  logInfo "Hello !" ("Some JSON payload" :: Text)
