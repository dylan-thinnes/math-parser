{-# LANGUAGE OverloadedStrings #-}
module Expr.Parse where

import Expr.Core
import Text.ParserCombinators.ReadP
import qualified Control.Monad.Combinators.Expr as CExpr

-- Operator Typeclass
class (Show a) => Operator a where
    symbols :: a -> [String]
    expr :: a -> Expr -> Expr -> Expr
    wrap :: (Monad m) => a -> (m (Expr -> Expr -> Expr)) -> CExpr.Operator m Expr

rop :: (Operator a) => a -> ReadP a
rop op = choice $ map (\s -> op <$ (skipSpaces >> string s)) $ symbols op

toTableEntry :: (Operator a) => a -> CExpr.Operator ReadP Expr
toTableEntry op = wrap op $ expr op <$ rop op

-- Binary Operators
rassoc, lassoc :: BinOp -> Bool
rassoc Exponentiate = True
rassoc _            = False
lassoc = not . rassoc

-- Unary Operators
pre, post :: UnOp -> Bool
pre Negate    = True
pre Not       = True
pre Factorial = False
post = not . pre
