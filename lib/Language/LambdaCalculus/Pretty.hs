module Language.LambdaCalculus.Pretty (
    printTerm
) where

import Language.LambdaCalculus.AST
import Language.LambdaCalculus.Pretty.Term
import Language.LambdaCalculus.Pretty.Types

printTerm :: Term -> String
printTerm = printTm []
