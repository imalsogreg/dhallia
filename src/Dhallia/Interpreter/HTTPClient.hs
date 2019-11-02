{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Dhallia.Interpreter.HTTPClient where

import qualified Algebra.Graph as Graph
import qualified Algebra.Graph.ToGraph as Graph
import           Control.Monad.Reader    (ReaderT, ask, runReaderT)
import qualified Control.Monad.IO.Class as IO
import qualified Data.Aeson as Aeson
import qualified Control.Monad as Monad
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Dhall
import           GHC.Generics (Generic)
import qualified Network.HTTP.Client     as HTTP
import qualified Network.HTTP.Client.TLS as HTTPS
import qualified Dhall.Core
import qualified Dhall.Pretty as Pretty
import qualified Dhall.JSON
import qualified Dhall.Map as Map
import qualified Dhall.JSONToDhall

import Dhallia.API
import Dhallia.Cache
import Dhallia.Cache.InMemory
import Dhallia.Expr

request :: Dhall.Type Request
request = Dhall.record $
  Request <$> Dhall.field "baseUrl" Dhall.strictText
          <*> Dhall.field "verb"    Dhall.strictText
          <*> Dhall.field "pathParts" (Dhall.list Dhall.strictText)
          <*> Dhall.field "queryParams" (Dhall.list queryParam)
          <*> Dhall.field "requestBody" (Dhall.maybe Dhall.strictText)
  where
    queryParam :: Dhall.Type QueryParam
    queryParam = Dhall.record $
      QueryParam <$> Dhall.field "key" Dhall.strictText
                 <*> Dhall.field "value" Dhall.strictText



runRequests :: API (Cache IO) -> Expr -> ReaderT HTTP.Manager IO (Maybe Expr)
runRequests api' inputE = do
 -- maybe (return ()) (\Cache{stats} -> IO.liftIO (stats >>= print)) (getCache api')
 cacheResult <- maybe (return Nothing)
                      (\Cache{get} -> IO.liftIO (get inputE))
                      (getCache api')
 case cacheResult of
   Just r  -> IO.liftIO (putStrLn "Cache Hit" >> return (Just r))
   Nothing -> do
    IO.liftIO (putStrLn "Cache Miss")
    r <- case api' of

      Raw api@RawAPI{outputType} -> do
        let req  = Dhall.Core.normalize (Dhall.Core.App (toRequest api) inputE)
        parsed' <- case (Dhall.rawInput @Maybe request req) of
                     Just req -> pure req
                     Nothing  -> error "failed to decode request"
      
        httpResp <- runHTTP parsed'
      
        let responseJSON = case Aeson.decode (body httpResp) of
              Nothing    -> error "response body was not valid JSON"
              Just jsVal -> jsVal :: Aeson.Value
        let dhallResp =
              case Dhall.JSONToDhall.dhallFromJSON Dhall.JSONToDhall.defaultConversion outputType responseJSON of
                Left e  -> error $ "DhallFromJSON error: " ++ show e
                Right x -> x
      
        return $ Just dhallResp
      
      MapIn api@MapInAPI{f,parent} -> do
        let requestInput = Dhall.Core.normalize (Dhall.Core.App f inputE)
        runRequests parent requestInput
      
      MapOut api@MapOutAPI{f,parent} -> do
        resp <- runRequests parent inputE
        return $ fmap (Dhall.Core.normalize . Dhall.Core.App f) resp
      
      Merge api@MergeAPI{f,parentA,parentB} -> do
        let (inputA, inputB) = apInputs inputE
        Just respA <- runRequests parentA inputA
        Just respB <- runRequests parentB inputB
        let dhallResp = Dhall.Core.normalize (Dhall.Core.App (Dhall.Core.App f respA) respB)
        case dhallResp of
          Dhall.Core.Some resp -> return $ Just resp
    maybe (return ())
          (\(Cache{..}, val) -> IO.liftIO (set inputE val))
          ((,) <$> getCache api' <*> r)
    return r

data QueryParam = QueryParam
 { key   :: Text.Text
 , value :: Text.Text
 } deriving (Eq, Show, Generic)

instance Dhall.FromDhall QueryParam

data Request = Request
 { baseUrl     :: Text.Text
 , verb        :: Text.Text
 , pathParts   :: [Text.Text]
 , queryParams :: [QueryParam]
 , requestBody :: Maybe Text.Text
 } deriving (Eq, Show, Generic)

data Response = Response
  { body :: LazyByteString.ByteString
  } deriving (Eq, Show, Generic)

instance Dhall.FromDhall Request


runHTTP :: Request -> ReaderT HTTP.Manager IO Response
runHTTP Request{..} = do
  mgr <- ask
  req <- makeRequest
  response <- IO.liftIO $ HTTP.httpLbs req mgr
  return $ Response (HTTP.responseBody response)
  where

    makeRequest = do
      let url     = baseUrl <> reqPath <> reqParams
          reqPath =
            if   null pathParts
            then ""
            else "/" <> (Text.intercalate "/" pathParts)
          reqParams =
            if   null queryParams
            then ""
            else "?" <> Text.intercalate "&" (fmap (\(QueryParam k v) -> k <> "=" <> v) queryParams)

      initReq <- HTTP.parseRequest (Text.unpack url)
      return $ initReq
        { HTTP.method = Text.encodeUtf8 verb
        , HTTP.requestBody = maybe "" (HTTP.RequestBodyBS .Text.encodeUtf8) requestBody
        }


test :: IO ()
test = do
  mgr <- HTTPS.newTlsManager
  x <- Dhall.inputExpr "./config/api.dhall"
  let deps = dependencyGraph x
  Monad.when (Graph.topSort deps == Nothing) (error  "found a cycle")

  example1Input <- Dhall.inputExpr "{ name = \"Taosie\" }"
  example2Input <- Dhall.inputExpr "{ name = \"Tao\" }"
  example3Input <- Dhall.inputExpr "{a = {name = \"Tao\"}, b = { name = \"Greg\"} }"
  apis <- getAPIs makeInMemory x
  r <- case Map.lookup "example3" apis of
    Just api -> runReaderT (runRequests api example3Input) mgr
  maybe (error "response error") (print . Pretty.prettyExpr) r


test2 :: IO ()
test2 = do
  mgr <- HTTPS.newTlsManager
  x <- Dhall.inputExpr "./examples/cat-facts.dhall"
  let deps = dependencyGraph x
  Monad.when (Graph.topSort deps == Nothing) (error  "found a cycle")
  exampleInput <- Dhall.inputExpr "{ _id = \"5d3e2191484b54001508b0df\" }"
  apis <- getAPIs makeInMemory x
  r <- case Map.lookup "cat-fact" apis of
    Just api -> runReaderT (runRequests api exampleInput) mgr
  maybe (error "response error") (print . Pretty.prettyExpr) r


test3 :: IO ()
test3 = do
  mgr <- HTTPS.newTlsManager
  x <- Dhall.inputExpr "./examples/metaweather.dhall"
  let deps = dependencyGraph x
  Monad.when (Graph.topSort deps == Nothing) (error  "found a cycle")
  exampleInput <- Dhall.inputExpr "\"san\""
  apis <- getAPIs makeInMemory x
  r <- case Map.lookup "mw-search" apis of
    Just api -> runReaderT (runRequests api exampleInput) mgr
  maybe (error "response error") (print . Pretty.prettyExpr) r


test4 :: IO String
test4 = do
  mgr <- HTTPS.newTlsManager
  x <- Dhall.inputExpr "./examples/cat-facts.dhall"
  let deps = dependencyGraph x
  Monad.when (Graph.topSort deps == Nothing) (error  "found a cycle")
  exampleInput <- Dhall.inputExpr "{}"
  apis <- getAPIs makeInMemory x
  r <- case Map.lookup "just-the-facts" apis of
    Just api -> runReaderT (runRequests api exampleInput) mgr
  maybe (error "response error") (return . show . Pretty.prettyExpr) r
