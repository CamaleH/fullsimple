module Language.LambdaCalculus.Types (
    Ty(..)
) where

import Language.LambdaCalculus.Symbol

data Ty = 
    TyBool
  | TyNat
  | TyArr Ty Ty 
  | TyBase Symbol
  deriving (Eq, Show)
