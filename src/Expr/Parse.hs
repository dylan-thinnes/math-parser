module Expr.Parse where

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
