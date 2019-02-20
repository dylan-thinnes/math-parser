module Expr.Core where

import Data.Functor.Foldable

-- Expr & Recursion Schemes
data Expr = Num Integer
          | BinaryExpr BinOp Expr Expr
          | UnaryExpr  UnOp Expr
    deriving (Eq, Show)

data ExprF a = NumF Integer
             | BinaryExprF BinOp a a
             | UnaryExprF  UnOp a
    deriving Functor

type instance Base Expr = ExprF

instance Recursive Expr where
    project :: Expr -> ExprF Expr
    project (Num i) = NumF i
    project (BinaryExpr op a b) = BinaryExprF op a b
    project (UnaryExpr op a) = UnaryExprF op a

instance Corecursive Expr where
    embed :: ExprF Expr -> Expr
    embed (NumF i) = Num i
    embed (BinaryExprF op a b) = BinaryExpr op a b
    embed (UnaryExprF op a) = UnaryExpr op a

-- Operators
data UnOp = Negate
    deriving (Eq, Enum, Ord, Bounded, Show, Read)

data BinOp = Exponentiate
           | Multiply
           | Divide
           | Add
           | Subtract
           | And
           | Xor
           | Or
           | Equals
    deriving (Eq, Enum, Ord, Bounded, Show, Read)

