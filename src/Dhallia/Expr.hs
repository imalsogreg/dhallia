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
