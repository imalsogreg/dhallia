{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns  #-}

module Dhallia.Cache.InMemory where

import qualified Control.Monad.IO.Class as IO
import qualified Data.Time.Clock as Time
import qualified Data.Time as Time
import qualified Data.IORef as IORef
import qualified Data.Map   as Map

import Dhallia.Expr
import Dhallia.Cache

data InMemoryCache = InMemoryCache
  { store            :: Map.Map Expr (Expr, Time.UTCTime)
  , lastInvalidation :: Time.UTCTime
  } deriving (Show)

emptyCache :: InMemoryCache
emptyCache = InMemoryCache
  { store            = mempty
  , lastInvalidation = Time.UTCTime
      { utctDay     = Time.fromGregorian 1979 1 1
      , utctDayTime = 0
      }
  }


makeInMemory :: IO.MonadIO m => CacheInfo -> IO (Cache m)
makeInMemory (CacheInfo ttlSeconds) = do
  c <- IORef.newIORef emptyCache

  let set k v = IO.liftIO $ do
        now <- Time.getCurrentTime
        let deadline = Time.addUTCTime (fromIntegral ttlSeconds) now
        IORef.modifyIORef c $ \InMemoryCache{..} ->
          InMemoryCache { store = Map.insert k (v, deadline) store, .. }

      get k   = IO.liftIO $ do
        now <- Time.getCurrentTime
        IORef.atomicModifyIORef c $ \InMemoryCache{..} ->
          case Map.lookup k store of

            -- No cache hit, no update needed
            Nothing -> (InMemoryCache{..}, Nothing)

            -- Cache hit when ttl expired, delete the key
            Just (_, ttlDeadline) |
              (lastInvalidation > ttlDeadline || now > ttlDeadline) ->
              (InMemoryCache { store = Map.delete k store, .. }, Nothing )

            -- Cache hit with ttl valid, return the key
            Just (v, ttlDeadline) | otherwise -> (InMemoryCache{..}, Just v)

      unsetAll = IO.liftIO $ do
        now <- Time.getCurrentTime
        let deadline = Time.addUTCTime (fromIntegral ttlSeconds) now
        IORef.modifyIORef c $ \InMemoryCache{..} ->
          InMemoryCache { lastInvalidation = deadline, .. }

      unset k = IO.liftIO $ do
        IORef.modifyIORef c $ \InMemoryCache{..} ->
          InMemoryCache { store = Map.delete k store, ..}

      stats = IO.liftIO $ IORef.readIORef c >>= \InMemoryCache{..} ->
        return (Stats { cacheEntries = Map.size store })
  return $ Cache { set, get, unsetAll, unset, stats }
