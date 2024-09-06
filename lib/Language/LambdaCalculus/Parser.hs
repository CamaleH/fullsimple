module Language.LambdaCalculus.Parser (
    parsing
) where

import Language.LambdaCalculus.AST
import Language.LambdaCalculus.Parser.Term
import Language.LambdaCalculus.Lexer
import Text.Parsec.Prim
import Data.Bifunctor
import Language.LambdaCalculus.ErrorInfo


parsing :: String -> [InfoToken] -> Either ErrorInfo Term
parsing filename s = first ParsingErr $ runParser parseTerm [] filename s
