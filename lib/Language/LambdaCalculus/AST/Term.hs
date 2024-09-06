module Language.LambdaCalculus.AST.Term (
    Info(..)
  , Term(..)
) where

import Language.LambdaCalculus.Types
import Language.LambdaCalculus.Symbol
import Text.Parsec.Pos

type Info = SourcePos

data Term = 
    TmVar Info Int Int 
  | TmAbs Info Symbol Ty Term
  | TmApp Info Term Term 
  | TmIf  Info Term Term Term 
  | TmTrue Info 
  | TmFalse Info 
  deriving (Show)

getInfo :: Term -> Info
getInfo (TmVar info _ _) = info
getInfo (TmAbs info _ _ _) = info
getInfo (TmApp info _ _) = info
getInfo (TmIf info _ _ _) = info
getInfo (TmTrue info ) = info
getInfo (TmFalse info ) = info
