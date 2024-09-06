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
    "(" ++ printTm ctx t1 ++ " " ++ printTm ctx t2 ++ ")"
  TmVar fi x n ->
    getBinding ctx x
  TmTrue _ -> "true"
  TmFalse _ -> "false"
