module Language.LambdaCalculus.ErrorInfo (
    ErrorInfo(..)
) where

import Text.Parsec.Error
import Language.LambdaCalculus.AST

data ErrorInfo = 
    LexingErr String
  | ParsingErr ParseError
  | TypeErr Info String

instance Show ErrorInfo where
  show (LexingErr msg) = "Error stage: lexing, msg: " ++ msg
  show (ParsingErr err) = "Error stage: parsing, msg: " ++ show err
  show (TypeErr fi msg) = msg ++ " at " ++ show fi
