module Language.LambdaCalculus.AST.Term (
    Info(..)
  , Term(..)
  , getInfo
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
  | TmZero Info
  | TmSucc Info Term
  | TmPred Info Term
  | TmIszero Info Term
  | TmUnit Info
  | TmAscri Info Term Ty
  deriving (Show)

getInfo :: Term -> Info
getInfo (TmVar info _ _) = info
getInfo (TmAbs info _ _ _) = info
getInfo (TmApp info _ _) = info
getInfo (TmIf info _ _ _) = info
getInfo (TmTrue info ) = info
getInfo (TmFalse info ) = info
getInfo (TmZero info) = info
getInfo (TmSucc info _) = info
getInfo (TmPred info _) = info
getInfo (TmIszero info _) = info
getInfo (TmUnit info) = info
