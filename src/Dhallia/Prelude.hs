-- | Extra functions useful for transforming API data
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dhallia.Prelude where

import qualified Control.Monad.IO.Class as IO
import qualified Data.List              as List
import           Data.Void              (Void)
import qualified Dhall
import qualified Dhall.Context          as Dhall
import qualified Dhall.Core             as Dhall
import qualified Dhall.Src              as Dhall

import           Dhallia.Expr

preludeContext :: Dhall.Context Expr
preludeContext =
  List.foldl' (\acc (k,v) -> Dhall.insert k v acc) Dhall.empty preludeTypes
  where
    preludeTypes = (\(DhalliaFunction{..} :: DhalliaFunction IO Void) -> (df_name, df_type)) <$> [ stringEquality ]

preludeNormalizer :: (Monad m, Eq a) => Dhall.NormalizerM m a
preludeNormalizer e0 = run [stringEquality]
  where
    run []     = return Nothing
    run (x:xs) = do
      r <- df_norm x e0
      case r of
        Nothing  -> run xs
        Just res -> return (Just res)

data DhalliaFunction m a = DhalliaFunction
  { df_type :: Dhall.Expr Dhall.Src Void
  , df_norm :: Dhall.NormalizerM m a
  , df_name :: Dhall.Text
  }

stringEquality :: Monad m => DhalliaFunction m a
stringEquality = DhalliaFunction
  { df_type = Dhall.Pi "_" Dhall.Text (Dhall.Pi "_" Dhall.Text Dhall.Bool)
  , df_name = "Text/equal"
  , df_norm = \case
      Dhall.App (Dhall.App (Dhall.Var "Text/equal")
                             (Dhall.TextLit (Dhall.Chunks _ x)))
                             (Dhall.TextLit (Dhall.Chunks _ y))
                 -> return . Just . Dhall.BoolLit $ x == y
      _ -> return Nothing
  }
