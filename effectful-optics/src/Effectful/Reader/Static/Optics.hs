module Effectful.Reader.Static.Optics
  ( rview,
  )
where

import Effectful (Eff, type (:>))
import Effectful.Reader.Static
import Optics.Core

rview :: (Reader s :> es, Is k A_Getter) => Optic' k is s a -> Eff es a
rview o = asks (view o)
{-# INLINE rview #-}