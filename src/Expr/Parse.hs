{-# LANGUAGE OverloadedStrings #-}
module Expr.Parse where

import Expr.Core
import Text.ParserCombinators.ReadP
import qualified Control.Monad.Combinators.Expr as CExpr

-- ============================ READING IN EXPRESSIONS ========================
equation :: ReadP Expr
equation = CExpr.makeExprParser rnum opTable

rnum :: ReadP Expr
rnum = do
    skipSpaces
    i <- readS_to_P reads
    if i < 0 
    then pfail
    else pure $ Num i

-- ============================ OPERATOR MANIPULATION =========================
-- Table of Operators
opTable, binTable, unTable :: [[CExpr.Operator ReadP Expr]]
opTable  = unTable ++ binTable
unTable  = map ((:[]).toTableEntry) (enumerate :: [UnOp])
binTable = map ((:[]).toTableEntry) (enumerate :: [BinOp])

enumerate :: (Bounded a, Enum a) => [a]
enumerate = [minBound .. maxBound]

-- Operator Typeclass
class (Show a) => Operator a where
    symbols :: a -> [String]
    expr :: a -> Expr -> Expr -> Expr
    wrap :: (Monad m) => a -> (m (Expr -> Expr -> Expr)) -> CExpr.Operator m Expr

instance Operator BinOp where
    symbols Exponentiate = ["**","^"]
    symbols Multiply     = ["*"]
    symbols Divide       = ["/"]
    symbols Add          = ["+"]
    symbols Subtract     = ["-"]
    symbols And          = ["&"]
    symbols Xor          = ["xor"]
    symbols Or           = ["|"]
    symbols Equals       = ["="]
    expr op = BinaryExpr op
    wrap op | rassoc op = CExpr.InfixR
            | lassoc op = CExpr.InfixL
            | otherwise = CExpr.InfixN

instance Operator UnOp where
    symbols Factorial = ["!"]
    symbols Not       = ["!","not"]
    symbols Negate    = ["-"]
    expr op = const $ UnaryExpr op
    wrap op | pre  op = CExpr.Prefix  . (<*> pure undefined)
            | post op = CExpr.Postfix . (<*> pure undefined)

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
