module Language.LambdaCalculus.Parser.Types (
    parseType
) where

import Language.LambdaCalculus.Parser.Common
import Language.LambdaCalculus.Lexer
import Language.LambdaCalculus.Token
import Language.LambdaCalculus.Types

import Data.ByteString.Lazy.Char8 as BS
import Text.Parsec.Prim
import Text.Parsec.Combinator

parseTyBool :: LCParser Ty 
parseTyBool = matchTok (TkId $ Symbol (0,BS.empty)) >> return TyBool

parseTyNat :: LCParser Ty 
parseTyNat = matchTok (TkId $ Symbol (3,BS.empty)) >> return TyNat

parseTyBase :: LCParser Ty
parseTyBase = TyBase <$> matchId

parseTyUnit :: LCParser Ty 
parseTyUnit = matchTok (TkId $ Symbol (4,BS.empty)) >> return TyUnit

parseNonTyArr :: LCParser Ty
parseNonTyArr = parens parseType
            <|> parseTyBool
            <|> parseTyNat
            <|> parseTyUnit
            <|> parseTyBase

parseTyArr :: LCParser (Ty -> Ty -> Ty)
parseTyArr = matchTok TkArrow >> return TyArr

parseType :: LCParser Ty
parseType = parseNonTyArr `chainr1` parseTyArr
