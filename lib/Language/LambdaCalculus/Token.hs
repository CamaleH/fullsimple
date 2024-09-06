module Language.LambdaCalculus.Token (
    Token(..)
  , Symbol(..)
) where

import Language.LambdaCalculus.Symbol

data Token = TkBackSlash 
           | TkId Symbol
           | TkDot
           | TkColon
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
    deriving (Show, Eq)
