module Effectful.State.Static.Local.Optics
  ( -- * General optics
    use,
    preuse,
    modifying,
    assign,

    -- * State modifying optics
    (.=),
    (?=),
    (%=),

    -- * State modifying optics with passthrough
    (%%=),

    -- * Returning new value
    (<.=),
    (<?=),
    (<%=),

    -- * Returning old value
    (<<.=),
    (<<?=),
    (<<%=),

    -- * Passthrough
    PermeableOptic (..),
  )
where

import Effectful (Eff, type (:>))
import Effectful.State.Static.Local
import Optics.Core
import Optics.Passthrough
import Optics.View

-- | Use the target of a 'Lens', 'Iso', or 'Getter' in the current state.
--
-- >>> evalState (use _1) ('a','b')
-- 'a'
--
-- >>> evalState (use _2) ("hello","world")
-- "world"
use :: (State s :> es, Is k A_Getter) => Optic' k is s a -> Eff es a
use o = gets (view o)
{-# INLINE use #-}

-- | Use the target of a 'AffineTraveral' or 'AffineFold' in the current state.
--
-- >>> evalState (preuse $ _1 % _Right) (Right 'a','b')
-- Just 'a'
--
-- @since 0.2
preuse :: (State s :> es, Is k An_AffineFold) => Optic' k is s a -> Eff es (Maybe a)
preuse o = gets (preview o)
{-# INLINE preuse #-}

-- | Map over the target(s) of an 'Optic' in our monadic state.
--
-- >>> execState (do modifying _1 (*10); modifying _2 $ stimes 5) (6,"o")
-- (60,"ooooo")
--
-- >>> execState (modifying each $ stimes 2) ("a","b")
-- ("aa","bb")
modifying :: (State t :> es, Is k A_Setter) => Optic k is t t a b -> (a -> b) -> Eff es ()
modifying o = modify . over o
{-# INLINE modifying #-}

-- | Replace the target(s) of an 'Optic' in our monadic state with a new value,
-- irrespective of the old.
--
-- >>> execState (do assign _1 'c'; assign _2 'd') ('a','b')
-- ('c','d')
--
-- >>> execState (assign each 'c') ('a','b')
-- ('c','c')
assign :: (State t :> es, Is k A_Setter) => Optic k is t t b a -> a -> Eff es ()
assign o = modifying o . const
{-# INLINE assign #-}

infix 4 .=, ?=, %=

-- | Replace the target(s) of an 'Optic' in our monadic state with a new value,
-- irrespective of the old.
--
-- This is an infix version of 'assign'.
(.=) :: (State t :> es, Is k A_Setter) => Optic k is t t b a -> a -> Eff es ()
(.=) = assign
{-# INLINE (.=) #-}

-- | Replace the target(s) of an 'Optic' in our monadic state with 'Just' a new
-- value, irrespective of the old.
(?=) :: (State t :> es, Is k A_Setter) => Optic k is t t b (Maybe a) -> a -> Eff es ()
(?=) = \o -> assign o . Just
{-# INLINE (?=) #-}

-- | Map over the target(s) of an 'Optic' in our monadic state.
--
-- This is an infix version of 'modifying'.
(%=) :: (State t :> es, Is k A_Setter) => Optic k is t t a b -> (a -> b) -> Eff es ()
(%=) = modifying
{-# INLINE (%=) #-}

infix 4 %%=

-- | Modify the target of an 'PermeableOptic' in the current state returning
-- some extra information of type depending on the optic (@r@, @Maybe r@ or
-- monoidal summary).
(%%=) ::
  (PermeableOptic k r, State s :> es) =>
  Optic k is s s a b ->
  (a -> (r, b)) ->
  Eff es (ViewResult k r)
o %%= f = state (passthrough o f)
{-# INLINE (%%=) #-}

infix 4 <.=, <?=, <%=

-- | Modify the target of a 'PermeableOptic' into your 'Monad''s state by a user
-- supplied function and return the result.
(<%=) ::
  (PermeableOptic k b, State s :> es) =>
  Optic k is s s a b ->
  (a -> b) ->
  Eff es (ViewResult k b)
o <%= f = o %%= \a -> let b = f a in (b, b)
{-# INLINE (<%=) #-}

-- | Set 'Just' a value with pass-through.
--
-- This is useful for chaining assignment without round-tripping through your
-- 'Monad' stack.
(<?=) ::
  (PermeableOptic k (Maybe b), State s :> es) =>
  Optic k is s s (Maybe a) (Maybe b) ->
  b ->
  Eff es (ViewResult k (Maybe b))
o <?= b = o <.= Just b
{-# INLINE (<?=) #-}

-- | Set with pass-through.
--
-- This is useful for chaining assignment without round-tripping through your
-- 'Monad' stack.
(<.=) ::
  (PermeableOptic k b, State s :> es) =>
  Optic k is s s a b ->
  b ->
  Eff es (ViewResult k b)
o <.= b = o <%= const b
{-# INLINE (<.=) #-}

infix 4 <<.=, <<?=, <<%=

-- | Modify the target of a 'PermeableOptic' into your 'Monad''s state by a user
-- supplied function and return the /old/ value that was replaced.
(<<%=) ::
  (PermeableOptic k a, State s :> es) =>
  Optic k is s s a b ->
  (a -> b) ->
  Eff es (ViewResult k a)
o <<%= f = o %%= \a -> (a, f a)
{-# INLINE (<<%=) #-}

-- | Replace the target of a 'PermeableOptic' into your 'Monad''s state with
-- 'Just' a user supplied value and return the /old/ value that was replaced.
(<<?=) ::
  (PermeableOptic k (Maybe a), State s :> es) =>
  Optic k is s s (Maybe a) (Maybe b) ->
  b ->
  Eff es (ViewResult k (Maybe a))
o <<?= b = o <<.= Just b
{-# INLINE (<<?=) #-}

-- | Replace the target of a 'PermeableOptic' into your 'Monad''s state with a
-- user supplied value and return the /old/ value that was replaced.
(<<.=) ::
  (PermeableOptic k a, State s :> es) =>
  Optic k is s s a b ->
  b ->
  Eff es (ViewResult k a)
o <<.= b = o <<%= const b
{-# INLINE (<<.=) #-}