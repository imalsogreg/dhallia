{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module API where

import qualified Data.Aeson as Aeson
import qualified Algebra.Graph
import           Algebra.Graph.ToGraph (topSort)
import qualified Control.Monad.IO.Class as Monad
import Control.Applicative ((<|>))
import qualified Data.Foldable as Foldable
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import qualified Data.Set as Set
import qualified Control.Monad as Monad
import qualified Data.String as String
import qualified Network.HTTP.Client     as HTTP
import qualified Network.HTTP.Client.TLS as HTTPS
import qualified Dhall
import qualified Dhall.JSON
import qualified Dhall.JSONToDhall
import qualified Data.Traversable as Traversable
import qualified Data.Maybe as Maybe
import qualified Dhall.Core
import qualified Dhall.Parser
import qualified Dhall.Pretty
import qualified Dhall.Src
import qualified Dhall.TypeCheck
import qualified Dhall.Map
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import BuiltinHTTP

data API s a =
    Raw (RawAPI s a)
  -- | Cmap CmapAPI
  | Fmap (FmapAPI s a)
  | Ap   (ApAPI s a)
  deriving (Eq, Show)



data RawAPI s a = RawAPI
  { name         :: Text.Text
  , inputType    :: Dhall.Core.Expr s a
    -- ^ A Dhall type
  , outputType   :: Dhall.Core.Expr s a
    -- ^ A Dhall type
  , toRequest    :: Dhall.Core.Expr s a
    -- ^ A Dhall function (input -> Request)
  , fromResponse :: Dhall.Core.Expr s a
    -- ^ A Dhall function (Response -> Optional output)
  } deriving (Eq, Show)

data FmapAPI s a = FmapAPI
  { name         :: Text.Text
  , parent       :: API s a
  , f            :: Dhall.Core.Expr s a
  , inputType    :: Dhall.Core.Expr s a
  , outputType   :: Dhall.Core.Expr s a
  } deriving (Eq, Show)

data ApAPI s a = ApAPI
  { name          :: Text.Text
  , parentA       :: API s a
  , parentB       :: API s a
  , f             :: Dhall.Core.Expr s a
  , inputType     :: Dhall.Core.Expr s a
  , outputType    :: Dhall.Core.Expr s a
  } deriving (Eq, Show)

newtype Exprs s a = Exprs
  { getExprs :: Dhall.Map.Map Text.Text (Dhall.Core.Expr s a) }


type E = Dhall.Core.Expr Dhall.Src.Src Dhall.TypeCheck.X
type A = API Dhall.Src.Src Dhall.TypeCheck.X

-- Extract names from the fields of a Dhall encoding of an API
getNamesAtFields :: E -> [Text.Text] -> [Text.Text]
getNamesAtFields (Dhall.Core.RecordLit e) fields =
  [ t
  | f <- fields
  , (Dhall.Core.TextLit (Dhall.Core.Chunks [] t)) <- Maybe.maybeToList $ Dhall.Map.lookup f e
  ]
getNamesAtFields _ _ = error "getNamesAtFields was applied to something other than a record"


dependencyGraph :: E -> Algebra.Graph.Graph (Text.Text, E)
dependencyGraph (Dhall.Core.RecordLit rs) =
  Foldable.foldl' (\g e -> componentsOf e `Algebra.Graph.overlay` g)
                  Algebra.Graph.empty
                  (Dhall.Map.toList rs)
  where
    componentsOf (name, e) =
      let dependencyNames = 
            getNamesAtFields e ["parent", "parentA", "parentB"]
          dependencies = Dhall.Map.restrictKeys rs (Set.fromList dependencyNames)
      in  Algebra.Graph.star (name,e) (Dhall.Map.toList dependencies)


getAPIs :: E -> Dhall.Map.Map Text.Text A
getAPIs e@(Dhall.Core.RecordLit rs) =
  let sortedExprs = Maybe.fromMaybe (error "Found cycle")  (topSort $ dependencyGraph e)

      getAPI
        :: (Text.Text, E)
        -> Dhall.Map.Map Text.Text A
        -> Dhall.Map.Map Text.Text A
      getAPI (name, Dhall.Core.RecordLit e) acc =
        Dhall.Map.insert name
        (Maybe.fromMaybe (error "decoding error") (fmap Raw getRaw <|> fmap Fmap getFmap <|> fmap  Ap getAp))
        acc
        where
          l fieldName = Dhall.Map.lookup fieldName e
          getRaw  = RawAPI  <$> pure name <*> l "inputType" <*> l "outputType" <*> l "toRequest" <*> l "fromResponse"
          getFmap = do
            Dhall.Core.TextLit (Dhall.Core.Chunks [] parentName) <- l "parent"
            parent  <- Dhall.Map.lookup parentName acc
            let inputType = getInputType parent
            outputType <- l "outputType"
            f <- l "f"
            return $ FmapAPI{..}
          getAp   = do
            Dhall.Core.TextLit (Dhall.Core.Chunks [] parentAName) <- l "parentA"
            Dhall.Core.TextLit (Dhall.Core.Chunks [] parentBName) <- l "parentB"
            parentA <- Dhall.Map.lookup parentAName acc
            parentB <- Dhall.Map.lookup parentBName acc
            let inputType = Dhall.Core.RecordLit $ Dhall.Map.fromList
                            [("a", getInputType parentA)
                            ,("b", getInputType parentB)
                            ] 
            f <- l "f"
            outputType <- l "outputType"
            return $ ApAPI{..}

      apis = Foldable.foldr getAPI mempty sortedExprs
  in  apis

getInputType :: A -> E
getInputType (Raw RawAPI{inputType}) = inputType
getInputType (Fmap FmapAPI{inputType}) = inputType
getInputType (Ap   ApAPI{inputType}) = inputType

getOutputType :: A -> E
getOutputType (Raw RawAPI{outputType}) = outputType
getOutputType (Fmap FmapAPI{outputType}) = outputType
getOutputType (Ap   ApAPI{outputType}) = outputType


showRequests :: A -> E -> IO ()
showRequests api' inputE = do
  case api' of
    Raw api ->
      print $ Dhall.Pretty.prettyExpr $ Dhall.Core.normalize (Dhall.Core.App (toRequest api) inputE)
    Fmap api ->
      showRequests (parent api) inputE
      -- print $ Dhall.Pretty.prettyExpr $ Dhall.Core.normalize (Dhall.Core.App (toRequest api) inputE)
    Ap api -> do
      let (inputA, inputB) = apInputs inputE
      showRequests (parentA api) inputA
      showRequests (parentB api) inputB

apInputs :: E -> (E, E)
apInputs (Dhall.Core.RecordLit (Dhall.Map.toList -> [("a",a),("b",b)])) = (a,b)
apInputs _ = error "Nope"
 
runRequests :: A -> E -> ReaderT HTTP.Manager IO (Maybe E)
runRequests api' inputE = case api' of

  Raw api@RawAPI{outputType} -> do
    let req  = Dhall.Core.normalize (Dhall.Core.App (toRequest api) inputE)
    
    httpResp <- runHTTP (Maybe.fromMaybe (error "Request decoding error") (Dhall.rawInput @Maybe request req))
    -- httpResp <- runHTTP (Maybe.fromMaybe (error "Request decoding error") _)
    Monad.liftIO $ print httpResp
    let Right dhallResp = Dhall.JSONToDhall.dhallFromJSON Dhall.JSONToDhall.defaultConversion outputType (Maybe.fromMaybe "Aeson decode error" $ Aeson.decode $ body httpResp)
    -- let dhallResp = Dhall.Core.RecordLit
    --                 (Dhall.Map.fromList [("body", String.fromString . Text.unpack $ body httpResp)])
    return $ Just dhallResp

    -- case Dhall.Core.normalize (Dhall.Core.App (fromResponse api) dhallResp) of
    --   Dhall.Core.Some resp -> do
    --     Monad.liftIO $ print $ Dhall.Pretty.prettyExpr resp
    --     return $ Just resp

  Fmap api@FmapAPI{f} -> do
    resp <- runRequests (parent api) inputE
    return $ fmap (Dhall.Core.normalize . Dhall.Core.App f) resp

  Ap api@ApAPI{f} -> do
    let (inputA, inputB) = apInputs inputE
    Just respA <- runRequests (parentA api) inputA 
    Just respB <- runRequests (parentB api) inputB
    let dhallResp = Dhall.Core.normalize (Dhall.Core.App (Dhall.Core.App f respA) respB)
    case dhallResp of
      Dhall.Core.Some resp -> return $ Just resp


test :: IO ()
test = do
  mgr <- HTTPS.newTlsManager
  x <- Dhall.inputExpr "./config/api.dhall"
  let deps = dependencyGraph x
  Monad.when (topSort deps == Nothing) (error  "found a cycle")

  example1Input <- Dhall.inputExpr "{ name = \"Taosie\" }"
  example2Input <- Dhall.inputExpr "{ name = \"Tao\" }"
  example3Input <- Dhall.inputExpr "{a = {name = \"Tao\"}, b = { name = \"Greg\"} }"
  r <- case Dhall.Map.lookup "example3" (getAPIs x) of
    Just api -> runReaderT (runRequests api example3Input) mgr

  maybe (error "response error") (print . Dhall.Pretty.prettyExpr) r

 
