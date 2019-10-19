{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BuiltinHTTP where

import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Reader
import qualified Data.ByteString.Lazy    as LBS
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.Encoding      as Text
import qualified Data.Text.IO
import qualified Dhall
import qualified Dhall.Context
import qualified Dhall.Context
import           Dhall.Core              (Expr (..), ReifiedNormalizer (..))
import qualified Dhall.Core              as Core
import qualified Dhall.Import            as DhallImport
import qualified Dhall.Map               as Map
import qualified Dhall.Map
import qualified Dhall.Pretty            as Dhall
import qualified Dhall.Parser            as DhallParser
import           Dhall.Src               (Src)
import           Dhall.TypeCheck         (X)
import           GHC.Generics            (Generic)
import qualified Lens.Family             as Lens
import qualified Network.HTTP.Client     as HTTP
import qualified Network.HTTP.Client.TLS

data QueryParam = QueryParam
 { key   :: Text
 , value :: Text
 } deriving (Eq, Show, Generic)

instance Dhall.Interpret QueryParam

data Request = Request
 { baseUrl     :: Text
 , verb        :: Text
 , pathParts   :: [Text]
 , queryParams :: [QueryParam]
 , requestBody :: Maybe Text
 } deriving (Eq, Show, Generic)

data Response = Response
  { body :: LBS.ByteString
  } deriving (Eq, Show, Generic)

instance Dhall.Interpret Request


-- instance Dhall.Interpret Response where
--   autoWith opts = fmap Response (Dhall.autoWith opts)

runHTTP :: Request -> ReaderT HTTP.Manager IO Response
runHTTP Request{..} = do
  mgr <- ask
  req <- makeRequest
  response <- liftIO $ HTTP.httpLbs req mgr
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


requestType = Core.Record $ Dhall.Map.fromList
  [ ("baseUrl", Core.Text)
  , ("verb"   , Core.Text)
  , ("pathParts", Core.App Core.List Core.Text)
  , ("queryParams", Core.App Core.List (Core.Record $ Dhall.Map.fromList
                                        [ ("key", Core.Text)
                                        , ("value", Core.Text)
                                        ]
                                       ))
  , ("requestBody", Core.App Core.Optional Core.Text)
  ]

responseType = Core.Record $ Dhall.Map.fromList
  [ ("body", Core.Text) ]

startingContext = transform Dhall.Context.empty
  where
    transform = Dhall.Context.insert "runHTTP" runHTTPType
    runHTTPType = Pi "_" requestType responseType

-- getText (

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


run :: Text.Text -> IO ()
run dhallString = do

  -- e <- Dhall.inputExpr dhallString
  let Right e = DhallParser.exprFromText "main" dhallString
  e' <- DhallImport.assertNoImports e
  mgr <- Network.HTTP.Client.TLS.newTlsManager
  -- x <- runReaderT (Core.normalizeWithM normalizerM e') mgr
  -- print (x :: Core.Expr X X)
  print e
  return ()

-- normalizer :: Core.ReifiedNormalizer X
-- normalizer (Core.

normalizerM :: Core.NormalizerM (ReaderT HTTP.Manager IO) X
normalizerM ( Core.App (Core.Var (Core.V "runHTTP" _))
              (x )
             -- (Core.RecordLit (Map.toList -> [("baseUrl", b@(Core.TextLit (Core.Chunks [] bUrl)))
             --                                ,("verb",Core.TextLit (Core.Chunks [] bVerb))
             --                                ,("pathParts", pParts)
             --                                ,("queryParams", qParams)
             --                                ,("requestBody", rBody)
             --                                ]))
            ) = do
  liftIO $ print "APP runHTTP"
  let Just r = Dhall.rawInput request x
  mgr <- ask
  resp <- runHTTP r
  undefined
  -- return $ Just $ RecordLit (Dhall.Map.singleton "body" (Core.TextLit (Core.Chunks [] (body resp)) ))
normalizerM (Core.Var (Core.V "runHTTP" _)) = do
  liftIO $ print "JUST runHTTP"
  error "hi"
normalizerM _ = do
  liftIO $ print "WILDCARD"
  return Nothing
