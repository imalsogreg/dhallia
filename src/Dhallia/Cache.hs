{-# LANGUAGE DeriveGeneric #-}

module Dhallia.Cache where

import qualified Dhall
import           GHC.Generics (Generic)

import           Dhallia.Expr

data Cache m = Cache
  { set      :: Expr -> Expr -> m ()
  , unset    :: Expr         -> m ()
  , unsetAll ::                 m ()
  , get      :: Expr         -> m (Maybe Expr)
  , stats    ::                 m Stats
  }

data Stats = Stats
  { cacheEntries :: Int
  } deriving (Eq, Show)

nullCache :: Monad m => Cache m
nullCache = Cache
  { set      = \_ _ -> return ()
  , unset    = \_   -> return ()
  , unsetAll =         return ()
  , get      = \_   -> return Nothing
  , stats    =         return (Stats 0)
  }

data CacheInfo = CacheInfo
  { ttlSeconds :: Integer
  } deriving (Eq, Generic, Show)

instance Dhall.FromDhall CacheInfo

cacheType :: Dhall.Type CacheInfo
cacheType = Dhall.autoWith Dhall.defaultInterpretOptions
