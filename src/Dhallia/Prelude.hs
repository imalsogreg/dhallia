-- | Extra values useful for transforming API data
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dhallia.Prelude where

import qualified Control.Monad.IO.Class as IO
import qualified Data.List              as List
import           Data.Proxy             (Proxy (..))
import qualified Data.Text              as Text
import qualified Data.Time              as Time
import qualified Data.Time.Calendar     as Time
import           Data.Void              (Void)
import qualified Dhall
import qualified Dhall.Context          as Dhall
import qualified Dhall.Core             as Dhall
import qualified Dhall.Map
import qualified Dhall.Src              as Dhall

import           Dhallia.Expr

inputExpr :: forall m. IO.MonadIO m => Text.Text -> m (Dhall.Expr Dhall.Src Void)
inputExpr txt = inputExprWithM preludeNormalizer (preludeContext (Proxy @m)) txt

preludeContext :: forall m.Monad m => Proxy m -> Dhall.Context Expr
preludeContext _ =
  List.foldl' (\acc (k,v) -> Dhall.insert k v acc) Dhall.empty preludeTypes
  where
    preludeTypes :: [(Dhall.Text, Dhall.Expr Dhall.Src Void)]
    preludeTypes =
      (("day", day) :) $
      (\(DhalliaValue{..}) -> (df_name, df_type)) <$> [ parseDay @m, stringEquality ]


preludeNormalizer :: (Monad m, Eq a) => Dhall.NormalizerM m a
preludeNormalizer e0 = run [parseDay, stringEquality]
  where
    run []     = return Nothing
    run (x:xs) = do
      r <- df_norm x e0
      case r of
        Nothing  -> run xs
        Just res -> return (Just res)


data DhalliaValue m a = DhalliaValue
  { df_type :: Dhall.Expr Dhall.Src Void
  , df_norm :: Dhall.NormalizerM m a
  , df_name :: Dhall.Text
  }


stringEquality :: Monad m => DhalliaValue m a
stringEquality = DhalliaValue
  { df_type = Dhall.Pi "_" Dhall.Text (Dhall.Pi "_" Dhall.Text Dhall.Bool)
  , df_name = "Text/equal"
  , df_norm = \case
      Dhall.App (Dhall.App (Dhall.Var "Text/equal")
                             (Dhall.TextLit (Dhall.Chunks _ x)))
                             (Dhall.TextLit (Dhall.Chunks _ y))
                 -> return . Just . Dhall.BoolLit $ x == y
      _ -> return Nothing
  }


day :: Dhall.Expr Dhall.Src Void
day = Dhall.Record
  (Dhall.Map.fromList
    [("year",   Dhall.Integer)
    , ("month", Dhall.Integer)
    , ("day",   Dhall.Integer)
    ])

parseDay :: Monad m => DhalliaValue m a
parseDay = DhalliaValue
  { df_type = Dhall.Pi "_" Dhall.Text day
  , df_name = "Day/parse"
  , df_norm = \e -> case (viewDateArgString e) >>= parse of
      Just t  -> return $ Just $ mkDay t
      Nothing -> return Nothing
      }
  where

    viewDateArgString :: Dhall.Expr s a -> Maybe Text.Text
    viewDateArgString
      (Dhall.App (Dhall.Var "Day/parse")
        (Dhall.TextLit (Dhall.Chunks _ x))) = Just x
    viewDateArgString _ = Nothing

    parse :: Text.Text -> Maybe Time.Day
    parse = Time.parseTimeM @Maybe False Time.defaultTimeLocale "%Y-%m-%d" . Text.unpack

    mkDay :: Time.Day -> Dhall.Expr s a
    mkDay (Time.toGregorian -> (y,m,d)) = Dhall.RecordLit
      (Dhall.Map.fromList
       [("year",  mkNat y)
       ,("month", mkNat m)
       ,("day",   mkNat d)
       ])
      where
        mkNat :: Integral i => i -> Dhall.Expr s a
        mkNat = Dhall.IntegerLit . fromIntegral


-- range :: Monad m => DhalliaValue m a
-- range = DhalliaValue
--   { df_type = Dhall.Pi "_" Dhall.Natural (Dhall.Pi "_" Dhall.Natural (Dhall.list Dhall.Natural))
--   , df_name = "List/range"
--   , df_norm = \e -> case e of
--       Dhall.App
--        (Dhall.App (Dhall.Var "List/range")
--                   (Dhall.NaturalLit i0))
--           (Dhall.NaturalLit i1)
--         -> Dhall.ListLit Nothing [Dhall.NaturalLit i | i <- [i0..i1]]
  -- }
