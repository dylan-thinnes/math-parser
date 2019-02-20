{-# LANGUAGE OverloadedStrings #-}
module Expr.Parse where

import Expr.Core

-- Operator Typeclass
class (Show a) => Operator a where
    symbols :: a -> [String]
    expr :: a -> Expr -> Expr -> Expr
    wrap :: (Monad m) => a -> (m (Expr -> Expr -> Expr)) -> CExpr.Operator m Expr

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
