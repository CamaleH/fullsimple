module Language.LambdaCalculus.Parser.Common (
    BoundContext
  , isnamebound
  , name2index
  , index2name
  , LCParser
  , matchTok
  , matchId
  , parens
) where

import Language.LambdaCalculus.Symbol
import Language.LambdaCalculus.AST
import Language.LambdaCalculus.Lexer(InfoToken(..))
import Language.LambdaCalculus.Token

import Text.Parsec.Prim
import Text.Parsec.Combinator
import Data.List

type BoundContext = [Symbol]

isnamebound :: BoundContext -> Symbol -> Bool 
isnamebound ctx n = foldr (\x b -> x == n || b) False ctx

index2name :: BoundContext -> Int -> Maybe Symbol
index2name [] _ = Nothing
index2name (x:xs) idx 
  | idx == 0 = Just x 
  |otherwise = index2name xs (idx-1)

name2index :: Symbol -> BoundContext -> Maybe Int
name2index = elemIndex 

type LCParser a = Parsec [InfoToken] BoundContext a

matchTok :: Token -> LCParser Token
matchTok x = token show info test 
  where
    test (InfoToken _ t) = if x == t then Just t else Nothing

matchId :: LCParser Symbol
matchId = token show info test 
  where
    test (InfoToken _ (TkId sym)) = Just sym
    test _ = Nothing

parens :: LCParser a -> LCParser a
parens p = do
  matchTok TkLpar
  res <- p
  matchTok TkRpar
  return res
