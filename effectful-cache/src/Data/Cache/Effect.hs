{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Strict #-}
{-|
  Module      : Data.Cache.Effect
  Copyright   : © Hécate Moonlight, 2021
  License     : MIT
  Maintainer  : hecate@glitchbra.in
  Stability   : stable

  An effect wrapper around Data.Cache for the Effectful ecosystem
-}
module Data.Cache.Effect
  ( -- * The /Cache/ effect
    Cache(..)
    -- * Handlers
  , runCacheIO
    -- * Cache operations
  , insert
  , lookup
  , keys
  , delete
  , filterWithKey
  ) where

import Control.Monad.IO.Class
import qualified Data.Cache as C
import Data.Hashable
import Data.Kind
import Effectful
import Prelude hiding (lookup)

-- | Operations on a cache
data Cache k v :: Effect where
  Insert :: (Eq k, Hashable k) => k -> v -> Cache k v m ()
  Lookup :: (Eq k, Hashable k) => k -> Cache k v m (Maybe v)
  Keys :: Cache k v m [k]
  Delete :: (Eq k, Hashable k) => k -> Cache k v m ()
  FilterWithKey :: (Eq k, Hashable k) => (k -> v -> Bool) -> Cache k v m ()

-- | The default IO handler
runCacheIO :: forall (k :: Type) (v :: Type) (es :: [Effect]) (a :: Type)
            . (Eq k, Hashable k, IOE :> es)
           => C.Cache k v
           -> Eff (Cache k v : es) a
           -> Eff es a
runCacheIO cache = interpret $ \case
  Insert key value  -> liftIO $ C.insert cache key value
  Lookup key        -> liftIO $ C.lookup cache key
  Keys              -> liftIO $ C.keys cache
  Delete key        -> liftIO $ C.delete cache key
  FilterWithKey fun -> liftIO $ C.filterWithKey fun cache

-- runCacheSTM :: (Eq k, Hashable k)
--             => C.Cache k v
--             -> Eff (Cache k v : es) a
--             -> Eff es a
-- runCurrentTimePure :: UTCTime -> Eff (Time : es) a -> Eff es a
-- runCurrentTimePure time = interpret $ \CurrentTime -> pure time

-- | Insert an item in the cache, using the default expiration value of the cache.
insert :: forall (k :: Type) (v :: Type) (es :: [Effect])
        . (Eq k, Hashable k, Cache k v :> es)
       => k -> v -> Eff es ()
insert key value = send $ Insert key value

-- | Lookup an item with the given key, and delete it if it is expired.
--
-- The function will only return a value if it is present in the cache and if the item is not expired.
-- The function will eagerly delete the item from the cache if it is expired.
lookup :: forall (k :: Type) (v :: Type) (es :: [Effect])
        . (Eq k, Hashable k, Cache k v :> es)
       => k -> Eff es (Maybe v)
lookup key = send $ Lookup key

-- | List all the keys of the cache.
--
-- You will need to specify what are the types of the key & value parameters since
-- we are in complex type-level wizardry around those parts.
--
-- === __Example__
--
-- > listKeys :: (Cache Int Int :> es) => Eff es [Int]
-- > listKeys = do
-- >   mapM_ (\(k,v) -> insert @Int @Int k v) [(2,4),(3,6),(4,8),(5,10)]
-- >   keys @Int @Int -- [2,3,4,5]
keys :: forall (k :: Type) (v :: Type) (es :: [Effect])
      . (Cache k v :> es) => Eff es [k]
keys = send @(Cache k v) Keys

-- | Delete the provided key from the cache it is present.
--
-- You will need to specify what are the types of the key & value parameters since
-- we are in complex type-level wizardry around those parts.
--
-- === __Example__
--
-- > deleteKeys :: (Cache Int Int :> es) => Eff es [Int]
-- > deleteKeys = do
-- >   mapM_ (\(k,v) -> insert @Int @Int k v) [(2,4),(3,6),(4,8),(5,10)]
-- >   delete @Int @Int 3
-- >   delete @Int @Int 5
-- >   keys @Int @Int -- [2,4]
delete :: forall (k :: Type) (v :: Type) (es :: [Effect])
        . (Eq k, Hashable k, Cache k v :> es)
      => k -> Eff es ()
delete key = send @(Cache k v) $ Delete key

-- | Keeps elements that satisfy the predicate (used for cache invalidation).
--
-- Note that the predicate might be called for expired items.
-- You will need to specify what are the types of the key & value parameters since
-- we are in complex type-level wizardry around those parts.
--
-- === __Example__
--
-- > filterKeys :: (Cache Int Int :> es) => Eff es [Int]
-- > filterKeys = do
-- >   mapM_ (\(k,v) -> insert @Int @Int k v) [(2,4),(3,6),(4,8),(5,10)]
-- >   filterWithKey @Int @Int (\k _ -> k /= 3)
-- >   keys @Int @Int -- [2,4,5]
filterWithKey :: forall (k :: Type) (v :: Type) (es :: [Effect])
               . (Eq k, Hashable k, Cache k v :> es)
              => (k -> v -> Bool) -> Eff es ()
filterWithKey fun = send $ FilterWithKey fun

