{-# LANGUAGE OverloadedStrings #-}
module Expr.Parse
    ( Expr(..)
    , ParseError
    , prettyPrint
    , parseToExpr
    ) where

import Expr.Core

-- Error handling utilities
import Data.List (intersperse)
import Data.Char (isSpace)

-- Equation parsing mechanisms
import Text.ParserCombinators.ReadP
import qualified Control.Monad.Combinators.Expr as CExpr
import Control.Applicative ((<|>))

-- Defining Read instance using ReadP
import Text.Read (readPrec)
import Text.ParserCombinators.ReadPrec (lift)

-- Pretty Printer
import Data.Functor.Foldable (cata)
import qualified Data.Text as T (concat, unpack, pack, Text)

-- ============================ ERROR HANDLING ================================
data ParseError = TooManyParses [Expr]
                | NoParses
                | ParseEnd String
    deriving (Eq)

instance Show ParseError where
    show (TooManyParses exprs) = "There were several parses: \n"
        ++ (concat $ intersperse "\n" $ map prettyPrint $ exprs)
    show (NoParses)            = "No parses could be found."
    show (ParseEnd rem)        = "Parse ended unexpectedly at: " ++ rem

parseToExpr :: String -> Either ParseError Expr
parseToExpr str = case validParses of
                 [] -> case invalidParses of
                         [] -> Left NoParses
                         ps -> Left $ ParseEnd $ snd $ last ps
                 [parse] -> Right $ fst parse
                 parses'     -> Left $ TooManyParses $ map fst parses'
    where
    parses = (reads :: ReadS Expr) str
    validParses   = filter remainderBlank parses
    invalidParses = filter (not . remainderBlank) parses
    remainderBlank = all isSpace . snd

prettyPrint :: Expr -> String
prettyPrint = T.unpack . cata f
    where
    f :: ExprF T.Text -> T.Text
    f (BinaryExprF op t1 t2)
      = T.concat ["(", t1, " ", T.pack $ head $ symbols op, " ", t2, ")"]
    f (UnaryExprF op t)
      | pre op
      = T.concat ["(", T.pack $ head $ symbols op, t, ")"]
      | post op
      = T.concat ["(", t, T.pack $ head $ symbols op, ")"]
    f (NumF i)
      = T.pack $ show i

-- ============================ READING IN EXPRESSIONS ========================
instance Read Expr where
    readPrec = lift equation

equation :: ReadP Expr
equation = CExpr.makeExprParser term opTable

term :: ReadP Expr
term = parens <|> rnum

parens :: ReadP Expr
parens = between 
    (skipSpaces >> string "(") 
    (skipSpaces >> string ")")
    equation

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
