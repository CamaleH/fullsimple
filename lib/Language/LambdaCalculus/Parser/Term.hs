module Language.LambdaCalculus.Parser.Term (
    parseTerm
) where

import Language.LambdaCalculus.Parser.Common
import Language.LambdaCalculus.Lexer
import Language.LambdaCalculus.Token
import Language.LambdaCalculus.Types
import Language.LambdaCalculus.AST

import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List
import Text.Parsec.Prim
import Text.Parsec.Combinator
import Language.LambdaCalculus.Parser.Types (parseType)
import Control.Monad(join)

parseTmVar :: LCParser Term
parseTmVar = 
  join $ findVar <$> getPosition <*> matchId <*> getState

findVar :: Info -> Symbol -> BoundContext -> LCParser Term
findVar info v list = case name2index v list of
  Nothing -> fail $ "The variable " ++ show v ++ " has not been bound"
  Just n  -> return $ TmVar info n (length list)

parseTmAbs :: LCParser Term
parseTmAbs = do
  info <- getPosition
  matchTok TkBackSlash
  v <- matchId
  oldctx <- getState
  putState (v:oldctx)
  matchTok TkColon
  ty <- parseType
  matchTok TkDot
  term <- parseTerm
  putState oldctx
  return $ TmAbs info v ty term

parseNonApp :: LCParser Term
parseNonApp =  parens parseTerm
           <|> parseTmAbs
           <|> parseTmIf
           <|> parseTmTrue
           <|> parseTmFalse
           <|> parseTmVar

parseTmIf :: LCParser Term
parseTmIf =
  TmIf <$> getPosition
       <*> (matchTok TkIf >> parseTerm)
       <*> (matchTok TkThen >> parseTerm)
       <*> (matchTok TkElse >> parseTerm)

parseTmTrue :: LCParser Term
parseTmTrue = do
  info <- getPosition
  matchTok (TkId $ Symbol (1,BS.empty))
  return $ TmTrue info

parseTmFalse :: LCParser Term
parseTmFalse = do
  info <- getPosition
  matchTok (TkId $ Symbol (2,BS.empty))
  return $ TmTrue info

parseTerm :: LCParser Term
parseTerm = chainl1 parseNonApp $ TmApp <$> getPosition
