{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Effectful.Monad
import Test.Hspec as H

import StdoutExample ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = return ()
