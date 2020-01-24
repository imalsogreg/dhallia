--------------------------------------------------------------------------------
-- |
-- Module : Dhallia.Expr
-- Copyright : (c) Greg Hale 2019
-- License   : BSD-3-Clause
-- Maintainer : imalsogreg@gmail.com
-- Stability : Experimental
-- Portability : posix
--
-- A custom version of Dhall's @inputExprWithM@ that allows normalization to
-- happen within a monad.
-- Dhall's buildin `inputExprWith` forces the use of pure normalizers
--------------------------------------------------------------------------------

{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}

module Dhallia.Expr (
  Expr
  , inputExprWithM
  ) where

import qualified Control.Monad.IO.Class     as IO
import qualified Control.Monad.State.Strict as State
import qualified Data.List                  as List
import           Data.Text                  (Text)
import           Data.Void                  (Void)
import qualified Dhall
import qualified Dhall.Context              as Dhall
import qualified Dhall.Core                 as Dhall
import qualified Dhall.Import
import qualified Dhall.Parser
import           Dhall.Src                  (Src)
import qualified Dhall.TypeCheck
import qualified Lens.Family

-- | Our @Expr@ specializes the one from @Dhall@ with the assumption
--   that expressions still have source spans and all imports have been resolved
type Expr = Dhall.Expr Src Void

-- |
inputExprWithM
  :: IO.MonadIO m => Dhall.NormalizerM m Void
  -> Dhall.Context Expr
  -> Text
  -> m Expr
inputExprWithM n ctx txt = do
  let settings = Dhall.defaultInputSettings
  expr <- Dhall.throws (Dhall.Parser.exprFromText (Lens.Family.view Dhall.sourceName settings) txt)
  let ctx' = List.foldl' (\acc (k,v) -> Dhall.insert k v acc)
                         (Lens.Family.view Dhall.startingContext settings)
                         (Dhall.toList ctx)
  let status = Lens.Family.set Dhall.Import.startingContext ctx (Dhall.Import.emptyStatus ".")
  expr' <- IO.liftIO $ State.evalStateT (Dhall.Import.loadWith expr) status
  _ <- Dhall.throws (Dhall.TypeCheck.typeWith ctx' expr')
  Dhall.normalizeWithM n expr'
