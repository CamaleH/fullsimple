module Language.LambdaCalculus.Context (
    Context
  , ctxLength
  , addBinding
  , getBinding
) where

import Language.LambdaCalculus.AST
import Language.LambdaCalculus.Types
import Language.LambdaCalculus.Symbol

type Context a = [a]

ctxLength :: Context a -> Int
ctxLength = length

addBinding :: a -> Context a -> Context a
addBinding = (:)

getBinding :: Context a -> Int -> a
getBinding ctx n = ctx !! n
