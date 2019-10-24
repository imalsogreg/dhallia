{-# LANGUAGE OverloadedStrings #-}

module CallAPI where

import qualified Data.Text as Text
import qualified Data.Aeson       as Aeson
import Data.Aeson ((.=))
import qualified Dhall            as Dhall
import qualified Dhall.Core       as Dhall
import qualified Dhall.TypeCheck  as Dhall
import qualified Dhall.Src        as Dhall
import qualified Dhall.JSONToDhall as J2D


data Request = Request
  { verb        :: Text.Text
  , pathParts   :: [Text.Text]
  , requestBody :: Maybe Text.Text
  }



call :: Text.Text -> Text.Text -> Aeson.Value -> IO Aeson.Value
call apiModule apiName inputJSON = do

  api <- Dhall.inputExpr apiModule
  print api

  return (Aeson.Number 1)

exampleInput = Aeson.object [ "name" .= Text.pack "DhallIA" ]

runExample = call "./config/c4.dhall" "example1" exampleInput
