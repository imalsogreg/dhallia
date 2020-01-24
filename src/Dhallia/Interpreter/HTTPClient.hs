{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeApplications      #-}

module Dhallia.Interpreter.HTTPClient where

import qualified Algebra.Graph.ToGraph   as Graph
import qualified Control.Monad           as Monad
import qualified Control.Monad.IO.Class  as IO
import           Control.Monad.Reader    (ReaderT, ask, runReaderT)
import qualified Data.Aeson              as Aeson
import qualified Data.ByteString.Lazy    as LazyByteString
import qualified Data.CaseInsensitive    as CI
import           Data.Proxy              (Proxy (..))
import qualified Data.Text               as Text
import qualified Data.Text.Encoding      as Text
import           Data.Void               (Void)
import qualified Dhall
import qualified Dhall.Core              as Dhall
import qualified Dhall.JSONToDhall
import qualified Dhall.Map               as Map
import qualified Dhall.Pretty            as Pretty
import           GHC.Generics            (Generic)
import qualified Network.HTTP.Client     as HTTP
import qualified Network.HTTP.Client.TLS as HTTPS

import           Dhallia.API
import           Dhallia.Cache
import           Dhallia.Cache.InMemory
import           Dhallia.Expr
import           Dhallia.Prelude         (inputExpr, preludeNormalizer)

request :: Dhall.Type Request
request = Dhall.record $
  Request <$> Dhall.field "baseUrl" Dhall.strictText
          <*> Dhall.field "verb"    Dhall.strictText
          <*> Dhall.field "pathParts" (Dhall.list Dhall.strictText)
          <*> Dhall.field "queryParams" (Dhall.list queryParam)
          <*> Dhall.field "requestBody" (Dhall.maybe Dhall.strictText)
          <*> Dhall.field "headers" (Dhall.list requestHeader)
  where
    queryParam :: Dhall.Type QueryParam
    queryParam = Dhall.record $
      QueryParam <$> Dhall.field "key"   Dhall.strictText
                 <*> Dhall.field "value" Dhall.strictText
    requestHeader :: Dhall.Type Header
    requestHeader = Dhall.record $
      Header <$> Dhall.field "key"   Dhall.strictText
             <*> Dhall.field "value" Dhall.strictText


runRequests :: API (Cache IO) -> Expr -> ReaderT HTTP.Manager IO (Maybe Expr)
runRequests = runRequestsWith preludeNormalizer

runRequestsWith :: Dhall.NormalizerM IO Void -> API (Cache IO) -> Expr -> ReaderT HTTP.Manager IO (Maybe Expr)
runRequestsWith n api' inputE = do
 cacheResult <- maybe (return Nothing)
                      (\Cache{get} -> IO.liftIO (get inputE))
                      (getCache api')
 case cacheResult of
   Just r  -> IO.liftIO (putStrLn "Cache Hit" >> return (Just r))
   Nothing -> do
    IO.liftIO (putStrLn "Cache Miss")
    r <- case api' of

      Raw api@RawAPI{outputType} -> do
        req     <- IO.liftIO $ Dhall.normalizeWithM n (Dhall.App (toRequest api) inputE)
        parsed' <- case (Dhall.rawInput @Maybe request req) of
                     Just req -> pure req
                     Nothing  -> error "failed to decode request"

        httpResp <- runHTTP parsed'

        -- IO.liftIO $ print (body httpResp)
        let responseJSON = case Aeson.decode (body httpResp) of
              Nothing    -> error "response body was not valid JSON"
              Just jsVal -> jsVal :: Aeson.Value
        let dhallResp =
              case Dhall.JSONToDhall.dhallFromJSON Dhall.JSONToDhall.defaultConversion outputType responseJSON of
                Left e  -> error $ "DhallFromJSON error: " ++ show e
                Right x -> x

        return $ Just dhallResp

      MapIn api@MapInAPI{f,parent} -> do
        requestInput <- IO.liftIO $ Dhall.normalizeWithM n (Dhall.App f inputE)
        runRequestsWith n parent requestInput

      MapOut api@MapOutAPI{f,parent} -> do
        resp <- runRequestsWith n parent inputE
        case resp of
          Nothing -> return Nothing
          Just r  -> fmap Just $ IO.liftIO $ Dhall.normalizeWithM n $ Dhall.App f r

      Merge api@MergeAPI{f,parentA,parentB} -> do
        let (inputA, inputB) = apInputs inputE
        Just respA <- runRequestsWith n parentA inputA
        Just respB <- runRequestsWith n parentB inputB
        dhallResp <- IO.liftIO $ Dhall.normalizeWithM n (Dhall.App (Dhall.App f respA) respB)
        case dhallResp of
          Dhall.Some resp -> return $ Just resp

    maybe (return ())
          (\(Cache{..}, val) -> IO.liftIO (set inputE val))
          ((,) <$> getCache api' <*> r)
    return r


data QueryParam = QueryParam
 { key   :: Text.Text
 , value :: Text.Text
 } deriving (Eq, Show, Generic)

instance Dhall.FromDhall QueryParam

data Header = Header
  { key   :: Text.Text
  , value :: Text.Text
  } deriving (Eq, Show, Generic)

instance Dhall.FromDhall Header


data Request = Request
 { baseUrl     :: Text.Text
 , verb        :: Text.Text
 , pathParts   :: [Text.Text]
 , queryParams :: [QueryParam]
 , requestBody :: Maybe Text.Text
 , headers     :: [Header]
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
      IO.liftIO $ print url
      return $ initReq
        { HTTP.method = Text.encodeUtf8 verb
        , HTTP.requestBody = maybe "" (HTTP.RequestBodyBS .Text.encodeUtf8) requestBody
        , HTTP.requestHeaders =
            fmap (\(Header k v) -> (CI.mk (Text.encodeUtf8 k), Text.encodeUtf8 v)) headers
        }


test :: IO ()
test = do
  mgr <- HTTPS.newTlsManager
  x <- inputExpr "./config/api.dhall"
  let deps = dependencyGraph x
  Monad.when (Graph.topSort deps == Nothing) (error  "found a cycle")

  example3Input <- Dhall.inputExpr "{a = {name = \"Tao\"}, b = { name = \"Greg\"} }"
  apis <- getAPIs makeInMemory x
  r <- case Map.lookup "example3" apis of
    Just api -> runReaderT (runRequests api example3Input) mgr
    Nothing  -> error "API lookup error"
  maybe (error "response error") (print . Pretty.prettyExpr) r


test2 :: IO ()
test2 = do
  mgr <- HTTPS.newTlsManager
  x <- inputExpr "./examples/cat-facts.dhall"
  let deps = dependencyGraph x
  Monad.when (Graph.topSort deps == Nothing) (error  "found a cycle")
  exampleInput <- Dhall.inputExpr "{ _id = \"5d3e2191484b54001508b0df\" }"
  apis <- getAPIs makeInMemory x
  r <- case Map.lookup "cat-fact" apis of
    Just api -> runReaderT (runRequests api exampleInput) mgr
    Nothing  -> error "API lookup error"
  maybe (error "response error") (print . Pretty.prettyExpr) r


test3 :: IO ()
test3 = do
  mgr <- HTTPS.newTlsManager
  x <- inputExpr "./examples/metaweather.dhall"
  let deps = dependencyGraph x
  Monad.when (Graph.topSort deps == Nothing) (error  "found a cycle")
  exampleInput <- Dhall.inputExpr "\"san\""
  apis <- getAPIs makeInMemory x
  r <- case Map.lookup "mw-search" apis of
    Just api -> runReaderT (runRequests api exampleInput) mgr
    Nothing  -> error "API lookup error"
  maybe (error "response error") (print . Pretty.prettyExpr) r


test4 :: IO String
test4 = do
  mgr <- HTTPS.newTlsManager
  x <- inputExpr "./examples/cat-facts.dhall"
  let deps = dependencyGraph x
  Monad.when (Graph.topSort deps == Nothing) (error  "found a cycle")
  exampleInput <- Dhall.inputExpr "{}"
  apis <- getAPIs makeInMemory x
  r <- case Map.lookup "just-the-facts" apis of
    Just api -> runReaderT (runRequests api exampleInput) mgr
    Nothing  -> error "API lookup error"
  maybe (error "response error") (return . show . Pretty.prettyExpr) r

test5 :: IO String
test5 = do
  mgr <- HTTPS.newTlsManager
  x <- inputExpr "../upstream/DFE_v8.dhall"
  let deps = dependencyGraph x
  Monad.when (Graph.topSort deps == Nothing) (error  "found a cycle")
  exampleInput <- Dhall.inputExpr "{ location = \"3801,2042,2224,758,254,1765,1536,1339,2754,2239,1354,1204,2739,83,2187,2426,1459,875,80,2243,770,1766,2237,1116,1368,1922,1962,1836,1430,2438,801,1837,1523,1531,1490,2093,1979,1953,1770,2112,1852,2429,2278,2342,1981,176,2572,2425,1785,1514,1061,2334,2220,2190,2467,1506,771,219,2374,2803,824\", tcin = \"75665658\", partial = True, horizon = \"5d1\" }"
  apis <- getAPIs (makeInMemory @IO) x
  r <- case Map.lookup "dfe_v8_stage" apis of
    Just api ->  do
      showRequests api exampleInput
      runReaderT (runRequests api exampleInput) mgr
    Nothing  -> error "API lookup error"
  -- maybe (error "response error") (return . show . Pretty.prettyExpr) r
  return "hi"

test6 :: IO ()
test6 = do
  mgr <- HTTPS.newTlsManager
  x <- inputExpr "./examples/cat-facts.dhall"
  let deps = dependencyGraph x
  Monad.when (Graph.topSort deps == Nothing) (error  "found a cycle")
  exampleInput <- Dhall.inputExpr "\"5d3e2191484b54001508b0df\""
  apis <- getAPIs makeInMemory x
  r <- case Map.lookup "cat-fact-easter-egg" apis of
    Just api -> runReaderT (runRequests api exampleInput) mgr
    Nothing  -> error "API lookup error"
  maybe (error "response error") (print . Pretty.prettyExpr) r
