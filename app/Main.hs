module Main where

import Language.LambdaCalculus.Lexer
import Language.LambdaCalculus.Parser.Term
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Language.LambdaCalculus.Parser
import Language.LambdaCalculus.TypeChecker
import Language.LambdaCalculus.Evaluator
import Language.LambdaCalculus.Pretty

main :: IO ()
main = putStrLn "Hello, Haskell!"

-- >>> lexing "stdin" $ BS.pack "\\x:Bool.x"
-- Right [InfoToken {info = (line 1, column 1), tk = TkBackSlash},InfoToken {info = (line 1, column 2), tk = TkId "x"},InfoToken {info = (line 1, column 3), tk = TkColon},InfoToken {info = (line 1, column 4), tk = TkId "Bool"},InfoToken {info = (line 1, column 8), tk = TkDot},InfoToken {info = (line 1, column 9), tk = TkId "x"},InfoToken {info = "stdin" (line 1, column 10), tk = TkEOF}]

-- >>> (lexing "stdin" (BS.pack "\\xxx:Bool. xxx") >>= parsing "stdin" >>= typeChecking)
-- Right (TyArr TyBool TyBool)

-- >>> printTerm.eval <$> (lexing "stdin" (BS.pack "\\xxx:Bool. xxx") >>= parsing "stdin")
-- Right "(\\\"xxx\":Bool.\"xxx\")"

-- >>> printTerm.eval <$> (lexing "stdin" (BS.pack "(\\x:Nat.succ x) 0") >>= parsing "stdin")
-- Right "succ 0"

-- >>> printTerm.eval <$> (lexing "stdin" (BS.pack "iszero ((\\x:Nat.succ x) 0)") >>= parsing "stdin")
-- Right "false"

-- >>> (lexing "stdin" (BS.pack "\\f:A->A.\\x:A.f(f(x))") >>= parsing "stdin" >>= typeChecking)
-- Right (TyArr (TyArr (TyBase "A") (TyBase "A")) (TyArr (TyBase "A") (TyBase "A")))
