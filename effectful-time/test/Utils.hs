module Utils where

import Control.Monad.IO.Class
import Effectful
import GHC.Stack
import qualified Test.Hspec as H

shouldBe :: (HasCallStack, Eq a, Show a, IOE :> es)
         => a -> a -> Eff es ()
shouldBe expected given = liftIO $ expected `H.shouldBe` given

