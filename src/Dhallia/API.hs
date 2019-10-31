{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE ViewPatterns          #-}

module Dhallia.API where

import qualified Algebra.Graph
import           Algebra.Graph.ToGraph   (topSort)
import           Control.Applicative     ((<|>))
import qualified Control.Monad           as Monad
import qualified Control.Monad.IO.Class  as Monad
import           Control.Monad.Reader    (ReaderT, ask, runReaderT)
import qualified Data.Aeson              as Aeson
import qualified Data.Foldable           as Foldable
-- import qualified Data.Map                as Map
import qualified Data.Maybe              as Maybe
import qualified Data.Set                as Set
import qualified Data.String             as String
import qualified Data.Text               as Text
import qualified Data.Text.IO            as Text
import qualified Data.Traversable        as Traversable
import           Data.Void               (Void)
import qualified Dhall
import qualified Dhall.Core

import qualified Dhall.Map as Map
import qualified Dhall.Parser
import qualified Dhall.Pretty
import qualified Dhall.Src
import qualified Dhall.TypeCheck


import           BuiltinHTTP

data API =
    Raw RawAPI
  -- | Cmap CmapAPI
  | MapOut MapOutAPI
  | Merge   MergeAPI
  deriving (Eq, Show)

type Expr = Dhall.Core.Expr Dhall.Src.Src Void

data RawAPI = RawAPI
  { name         :: Text.Text
  , inputType    :: Expr
    -- ^ A Dhall type
  , outputType   :: Expr
    -- ^ A Dhall type
  , toRequest    :: Expr
    -- ^ A Dhall function (input -> Request)
  } deriving (Eq, Show)

data MapOutAPI = MapOutAPI
  { name       :: Text.Text
  , parent     :: API
  , f          :: Expr
  , inputType  :: Expr
  , outputType :: Expr
  } deriving (Eq, Show)

data MergeAPI = MergeAPI
  { name       :: Text.Text
  , parentA    :: API
  , parentB    :: API
  , f          :: Expr
  , inputType  :: Expr
  , outputType :: Expr
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


getAPIs :: Expr -> Map.Map Text.Text API
getAPIs e@(Dhall.Core.RecordLit rs) =
  let sortedExprs = Maybe.fromMaybe (error "Found cycle")  (topSort $ dependencyGraph e)

      getAPI
        :: (Text.Text, Expr)
        -> Map.Map Text.Text API
        -> Map.Map Text.Text API
      getAPI (name, Dhall.Core.RecordLit e) acc =
        Map.insert name
        (Maybe.fromMaybe (error "api decoding error") (fmap Raw getRaw <|> fmap MapOut getMapOut <|> fmap  Merge getMerge))
        acc
        where
          l fieldName = Map.lookup fieldName e
          getRaw  = RawAPI <$> pure name <*> l "inputType" <*> l "outputType" <*> l "toRequest"
          getMapOut = do
            Dhall.Core.TextLit (Dhall.Core.Chunks [] parentName) <- l "parent"
            parent  <- Map.lookup parentName acc
            let inputType = getInputType parent
            outputType <- l "outputType"
            f <- l "f"
            return $ MapOutAPI{..}
          getMerge   = do
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

      apis = Foldable.foldr getAPI mempty sortedExprs
  in  apis

getInputType :: API -> Expr
getInputType (Raw  RawAPI{inputType})   = inputType
getInputType (MapOut MapOutAPI{inputType}) = inputType
getInputType (Merge   MergeAPI{inputType})   = inputType

getOutputType :: API -> Expr
getOutputType (Raw     RawAPI{outputType})    = outputType
getOutputType (MapOut  MapOutAPI{outputType}) = outputType
getOutputType (Merge   MergeAPI{outputType})  = outputType


showRequests :: API -> Expr -> IO ()
showRequests api' inputE = do
  case api' of
    Raw api ->
      print $ Dhall.Pretty.prettyExpr $ Dhall.Core.normalize (Dhall.Core.App (toRequest api) inputE)
    MapOut api ->
      showRequests (parent api) inputE

    Merge api -> do
      let (inputA, inputB) = apInputs inputE
      showRequests (parentA api) inputA
      showRequests (parentB api) inputB


apInputs :: Expr -> (Expr, Expr)
apInputs (Dhall.Core.RecordLit (Map.toList -> [("a",a),("b",b)])) = (a,b)
apInputs _ = error "Nope"
