module Dhallia.Expr where

import qualified Dhall
import qualified Dhall.Core
import qualified Dhall.Src  (Src)
import           Data.Void  (Void)

type Expr = Dhall.Core.Expr Dhall.Src.Src Void
