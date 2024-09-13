module Language.LambdaCalculus.Pretty.Term (
    printTm
) where

import Language.LambdaCalculus.AST
import Language.LambdaCalculus.Pretty.Types
import Language.LambdaCalculus.Context
import Language.LambdaCalculus.Symbol

printTm :: Context String -> Term -> String
printTm ctx t = case t of 
  TmAbs _ x ty t1 -> 
    let ctx' = addBinding (show x) ctx
    in "(\\" ++ show x ++ ":" ++ printType ty ++ "." ++ printTm ctx' t1 ++ ")"
  TmApp _ t1 t2 ->
    "(" ++ printTm ctx t1 ++ ") (" ++ printTm ctx t2 ++ ")"
  TmVar fi x n ->
    getBinding ctx x
  TmTrue _ -> "true"
  TmFalse _ -> "false"
  TmZero _ -> "0"
  TmSucc _ t1 -> "succ " ++ printTm ctx t1
  TmPred _ t1 -> "pred " ++ printTm ctx t1
  TmIszero _ t1 -> "iszero " ++ printTm ctx t1
  TmUnit _ -> "unit"
  TmLet _ x t1 t2 -> 
    let ctx' = addBinding (show x) ctx
    in "(let " ++ show x ++ "=" ++ printTm ctx t1 ++ " in " ++ printTm ctx' t2 ++ ")"
