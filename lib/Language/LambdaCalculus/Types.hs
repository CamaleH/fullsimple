module Language.LambdaCalculus.Types (
    Ty(..)
) where

data Ty = 
    TyBool
  | TyNat
  | TyArr Ty Ty 
  deriving (Eq, Show)
