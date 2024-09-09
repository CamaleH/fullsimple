{
module Language.LambdaCalculus.Lexer (
    InfoToken(..)
  , lexing
) where

import Language.LambdaCalculus.Token
import Language.LambdaCalculus.AST.Term (Info(..))
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Control.Monad (when)
import qualified Data.HashMap as HM
import Text.Parsec.Pos
import Language.LambdaCalculus.ErrorInfo
import Data.Bifunctor
}

%wrapper "monadUserState-bytestring"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters

@id = $alpha ($alpha | $digit | \')*

tokens :-

<0>  $white+                        ;
-- Keywords
<0>  "."                            { tok TkDot }
<0>  ":"                            { tok TkColon }
<0>  ";"                            { tok TkSemiColon }
<0>  "->"                           { tok TkArrow }
<0>  "\"                            { tok TkBackSlash }
<0>  "("                            { tok TkLpar }
<0>  ")"                            { tok TkRpar }
<0>  if                             { tok TkIf }
<0>  then                           { tok TkThen }
<0>  else                           { tok TkElse }
<0>  "0"                            { tok TkZero }
<0>  succ                           { tok TkSucc }
<0>  pred                           { tok TkPred }
<0>  iszero                         { tok TkIszero }
<0>  as                             { tok TkAs }
-- Identifiers
<0>  @id                            { tokId }

{
-- Each action has type :: String -> Token

-- The token type:
-- Token from Language.LambdaCalculus.Token

data AlexUserState = AlexUserState 
  { nextSym :: Int 
  , table :: HM.Map ByteString Int 
  }

alexInitUserState = AlexUserState 
  { nextSym = 4
  , table = HM.fromList [(BS.pack "Bool",0)
                        ,(BS.pack "true",1)
                        ,(BS.pack "false",2)
                        ,(BS.pack "Nat", 3)
                        ,(BS.pack "Unit", 4)
                        ,(BS.pack "unit", 5)]
  }

data InfoToken = InfoToken
  { info :: Info
  , tk   :: Token
  }
  deriving(Show, Eq)

setSource :: String -> InfoToken -> InfoToken
setSource name ik = ik{info = setSourceName (info ik) name}

mkInfoToken (AlexPn _ r c) k = InfoToken 
  { info = newPos "" r c 
  , tk = k }

tok :: Token -> AlexAction InfoToken
tok ctor (pos,_,_,_) _ = 
  return $ mkInfoToken pos ctor

tokId :: AlexAction InfoToken 
tokId (pos,_,str,_) len = 
  do 
    let name = BS.take len str 
    i <- do 
      state <- alexGetUserState
      let hashtable = table state 
      case HM.lookup name hashtable of 
        Just i -> return i 
        Nothing -> 
          let nextsym = nextSym state 
              nextsym' = nextsym + 1
              newtable = HM.insert name nextsym hashtable 
          in do 
            alexSetUserState state{ nextSym = nextsym', table = newtable }
            return nextsym 
    return $ mkInfoToken pos (TkId $ Symbol (i, name))

alexEOF :: Alex InfoToken
alexEOF = do
  (pos,_,_,_) <- alexGetInput
  return $ mkInfoToken pos TkEOF

lexing :: String -> ByteString -> Either ErrorInfo [InfoToken]
lexing filename input = first LexingErr $ runAlex input go
  where
    go = do 
      output <- alexMonadScan 
      if tk output == TkEOF 
        then return [setSource filename output]
        else (output :) <$> go
}
