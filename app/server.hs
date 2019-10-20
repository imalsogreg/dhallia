{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text.Encoding as Text
import qualified Data.Aeson as Aeson
import qualified Data.Foldable as Foldable
import qualified Data.ByteString as ByteString
import qualified Data.CaseInsensitive as CI
import Data.List
import System.Environment
import Snap.Http.Server
import Snap.Internal.Http.Server.Config as SnapCfg
import Snap.Core

type Header = (CI.CI ByteString.ByteString, ByteString.ByteString)

main :: IO ()
main = do
  args <- getArgs
  let port = case args of
        [pStr] -> read pStr :: Int
        _      -> 8080
  httpServe (cfg port) go
  where
    cfg port = SnapCfg.defaultConfig
          { port = Just port
          , bind = Just "127.0.0.1"
          }

go :: Snap ()
go = route [("greet/:name",greeter)
           ,("headers",echoHeaders)
           ]
  where
    greeter = do
      Just name <- getParam "name"
      writeLBS (Aeson.encode (Aeson.object [ "greet" Aeson..= ("Hi " <> Text.decodeUtf8 name) ]))

    echoHeaders :: Snap ()
    echoHeaders =
      let
        showHeader :: Header -> ByteString.ByteString
        showHeader (k,v) = (CI.original k) <> ":" <> v

        headerString :: [Header] -> ByteString.ByteString
        headerString =
          ByteString.intercalate "; " . map showHeader
      in getRequest >>= writeBS . headerString . listHeaders

-- Local Variables:
-- mode: haskell
-- End:
