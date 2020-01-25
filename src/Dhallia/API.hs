--------------------------------------------------------------------------------
-- |
-- Module      : Dhallia.API
-- Description : The encoding of APIs in dhall syntax-}
-- Copyright   : (c) Greg Hale 2019
-- License     : BSD-3-Clause
-- Maintainer  : imalsogreg@gmail.com
-- Stability   : experimental
-- Portability : Posix

-- API definitions are specified in Dhall (with some dhallia extensions).
-- This module defines the types of APIs that can be defined (Raw, MapIn, Merge, etc),
-- and the method for decoding them from Dhall into Haskell.
--------------------------------------------------------------------------------

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE ViewPatterns          #-}

module Dhallia.API where

import qualified Algebra.Graph
import           Algebra.Graph.ToGraph  (topSort)
import           Control.Applicative    ((<|>))
import qualified Control.Monad.IO.Class as IO
import qualified Data.Foldable          as Foldable
import qualified Data.Maybe             as Maybe
import qualified Data.Set               as Set
import qualified Data.Text              as Text
import           Data.Void              (Void)
import qualified Dhall
import qualified Dhall.Core

import qualified Dhall.Map              as Map
import qualified Dhall.Pretty

import           Dhallia.Cache
import           Dhallia.Expr           (Expr)
import           Dhallia.Prelude        (preludeContext, preludeNormalizer)


data API c =
    Raw    (RawAPI c)
  | MapIn  (MapInAPI c)
  | MapOut (MapOutAPI c)
  | Merge  (MergeAPI c)
  deriving (Eq, Show)


data RawAPI c = RawAPI
  { name       :: Text.Text
  , inputType  :: Expr
    -- ^ A Dhall type
  , outputType :: Expr
    -- ^ A Dhall type
  , toRequest  :: Expr
    -- ^ A Dhall function (input -> Request)
  , cache      :: Maybe c
  } deriving (Eq, Show)


data MapInAPI c = MapInAPI
  { name       :: Text.Text
  , parent     :: API c
  , f          :: Expr
  , inputType  :: Expr
  , outputType :: Expr
  , cache      :: Maybe c
  } deriving (Eq, Show)


data MapOutAPI c = MapOutAPI
  { name       :: Text.Text
  , parent     :: API c
  , f          :: Expr
  , inputType  :: Expr
  , outputType :: Expr
  , cache      :: Maybe c
  } deriving (Eq, Show)

data MergeAPI c = MergeAPI
  { name       :: Text.Text
  , parentA    :: API c
  , parentB    :: API c
  , f          :: Expr
  , inputType  :: Expr
  , outputType :: Expr
  , cache      :: Maybe c
  } deriving (Eq, Show)


-- | Extract names from the fields of a Dhall encoding of an API
getNamesAtFields :: Expr -> [Text.Text] -> [Text.Text]
getNamesAtFields (Dhall.Core.RecordLit e) fields =
  [ t
  | f <- fields
  , (Dhall.Core.TextLit (Dhall.Core.Chunks [] t)) <- Maybe.maybeToList $ Map.lookup f e
  ]
getNamesAtFields _ _ = error "getNamesAtFields was applied to something other than a record"


dependencyGraph :: Expr -> Algebra.Graph.Graph (Text.Text, Expr)
dependencyGraph (Dhall.Core.RecordLit rs) =
  Foldable.foldl' (\g e -> componentsOf e `Algebra.Graph.overlay` g)
                  Algebra.Graph.empty
                  (Map.toList rs)
  where
    componentsOf (name, e) =
      let dependencyNames =
            getNamesAtFields e ["parent", "parentA", "parentB"]
          dependencies = Map.restrictKeys rs (Set.fromList dependencyNames)
      in  Algebra.Graph.star (name,e) (Map.toList dependencies)


getAPIs :: forall c.(CacheInfo -> IO c) -> Expr -> IO (Map.Map Text.Text (API c))
getAPIs makeCache e@(Dhall.Core.RecordLit rs) =
  let sortedExprs = Maybe.fromMaybe (error "Found cycle")  (topSort $ dependencyGraph e)

      getAPI
        :: (Text.Text, Expr)
        -> Map.Map Text.Text (API c)
        -> IO (Map.Map Text.Text (API c))
      getAPI (name, Dhall.Core.RecordLit e) acc = do

        cache <- case Map.lookup "cache" e of
          Nothing -> return Nothing
          Just e  -> do
            print (Dhall.Pretty.prettyExpr e)
            let cacheInfo = Maybe.fromMaybe (error "CacheInfo decoding error") $
                            Dhall.rawInput @Maybe (Dhall.maybe cacheType) e
            maybe (return Nothing) (fmap Just . makeCache) cacheInfo

        return $
          Map.insert name
          (Maybe.fromMaybe (error "api decoding error")
            (fmap Raw    (getRaw cache)    <|>
             fmap MapIn  (getMapIn cache)  <|>
             fmap MapOut (getMapOut cache) <|>
             fmap Merge  (getMerge cache)
            )
          )
          acc
        where
          l fieldName  = Map.lookup fieldName e

          getRaw cache = do
            RawAPI <$> pure name <*> l "inputType" <*> l "outputType" <*> l "toRequest" <*> pure cache
          getMapIn cache = do
            Dhall.Core.TextLit (Dhall.Core.Chunks [] parentName) <- l "parent"
            parent  <- Map.lookup parentName acc
            let outputType = getOutputType parent
            inputType <- l "inputType"
            f <- l "f"
            return $ MapInAPI{..}
          getMapOut cache = do
            Dhall.Core.TextLit (Dhall.Core.Chunks [] parentName) <- l "parent"
            parent  <- Map.lookup parentName acc
            let inputType = getInputType parent
            outputType <- l "outputType"
            f <- l "f"
            return $ MapOutAPI{..}
          getMerge cache = do
            Dhall.Core.TextLit (Dhall.Core.Chunks [] parentAName) <- l "parentA"
            Dhall.Core.TextLit (Dhall.Core.Chunks [] parentBName) <- l "parentB"
            parentA <- Map.lookup parentAName acc
            parentB <- Map.lookup parentBName acc
            let inputType = Dhall.Core.RecordLit $ Map.fromList
                            [("a", getInputType parentA)
                            ,("b", getInputType parentB)
                            ]
            f <- l "f"
            outputType <- l "outputType"
            return $ MergeAPI{..}

      apis = Foldable.foldrM getAPI mempty sortedExprs
  in  apis

getInputType :: API c -> Expr
getInputType (Raw    RawAPI{inputType})    = inputType
getInputType (MapIn  MapInAPI{inputType})  = inputType
getInputType (MapOut MapOutAPI{inputType}) = inputType
getInputType (Merge  MergeAPI{inputType})  = inputType

getOutputType :: API c -> Expr
getOutputType (Raw    RawAPI{outputType})    = outputType
getOutputType (MapIn  MapInAPI{outputType})  = outputType
getOutputType (MapOut MapOutAPI{outputType}) = outputType
getOutputType (Merge  MergeAPI{outputType})  = outputType

getCache :: API c -> Maybe c
getCache (Raw    RawAPI{cache})    = cache
getCache (MapOut MapOutAPI{cache}) = cache
getCache (MapIn MapInAPI{cache})   = cache
getCache (Merge MergeAPI{cache})   = cache

showRequests :: IO.MonadIO m => API c -> Expr -> m ()
showRequests = showRequestsWith preludeNormalizer

showRequestsWith :: (IO.MonadIO m) => Dhall.Core.Normalizer Void -> API c -> Expr -> m ()
showRequestsWith n api' inputE = do
  let n' = Just $ Dhall.Core.ReifiedNormalizer n
  case api' of

    Raw RawAPI{toRequest} -> do
      let req = Dhall.Core.normalizeWith n' (Dhall.Core.App toRequest inputE)
      IO.liftIO . print . Dhall.Pretty.prettyExpr $ req

    MapIn MapInAPI{f,parent} -> do
      let requestInput = Dhall.Core.normalizeWith n' (Dhall.Core.App f inputE)
      showRequestsWith n parent requestInput

    MapOut MapOutAPI{parent} ->
      showRequestsWith n parent inputE

    Merge api -> do
      let (inputA, inputB) = apInputs inputE
      showRequestsWith n (parentA api) inputA
      showRequestsWith n (parentB api) inputB


apInputs :: Expr -> (Expr, Expr)
apInputs (Dhall.Core.RecordLit (Map.toList -> [("a",a),("b",b)])) = (a,b)
apInputs _ = error "Nope"
