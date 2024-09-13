module Language.LambdaCalculus.Token (
    Token(..)
  , Symbol(..)
) where

import Language.LambdaCalculus.Symbol

data Token = TkBackSlash 
           | TkId Symbol
           | TkDot
           | TkColon
           | TkSemiColon
           | TkArrow
           | TkLpar
           | TkRpar
           | TkIf
           | TkThen
           | TkElse
           | TkEOF
           | TkZero
           | TkSucc
           | TkPred
           | TkIszero
           | TkAs
           | TkLet
           | TkAssign
           | TkIn
    deriving (Show, Eq)
