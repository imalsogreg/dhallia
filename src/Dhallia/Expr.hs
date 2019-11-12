{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}

module Dhallia.Expr where

import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.State.Strict    as State
import qualified Data.List              as List
import           Data.Text              (Text)
import           Data.Void              (Void)
import qualified Dhall
import qualified Dhall.Context          as Dhall
import qualified Dhall.Core             as Dhall
import qualified Dhall.Import
import qualified Dhall.Parser
import           Dhall.Src              (Src)
import qualified Dhall.TypeCheck
import qualified Lens.Family

type Expr = Dhall.Expr Src Void

inputExprWithM
  :: IO.MonadIO m => Dhall.NormalizerM m Void
  -> Dhall.Context Expr
  -> Text
  -> m (Dhall.Expr Src Void)
inputExprWithM n ctx txt = do
  let settings = Dhall.defaultInputSettings
  -- IO.liftIO $ print "About to exprFromText"
  expr <- Dhall.throws (Dhall.Parser.exprFromText (Lens.Family.view Dhall.sourceName settings) txt)
  let ctx' = List.foldl' (\acc (k,v) -> Dhall.insert k v acc)
                         (Lens.Family.view Dhall.startingContext settings)
                         (Dhall.toList ctx)
  let status = Lens.Family.set Dhall.Import.startingContext ctx (Dhall.Import.emptyStatus ".")
  -- IO.liftIO $ print "About to load"
  expr' <- IO.liftIO $ State.evalStateT (Dhall.Import.loadWith expr) status
  -- expr' <- IO.liftIO $ Dhall.Import.load expr
  -- IO.liftIO $ print "About to typecheck"
  _ <- Dhall.throws (Dhall.TypeCheck.typeWith ctx' expr')
  -- IO.liftIO $ print "About to normalize"
  Dhall.normalizeWithM n expr'
