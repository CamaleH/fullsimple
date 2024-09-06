module Language.LambdaCalculus.Symbol (
  Symbol(..)
) where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS

newtype Symbol = Symbol (Int, ByteString)

instance Eq Symbol where
  Symbol (n,_) == Symbol (m,_) = n==m

instance Ord Symbol where 
  compare (Symbol (n,_)) (Symbol (m,_)) = compare n m

instance Show Symbol where 
  show (Symbol (_,bs)) = show bs
