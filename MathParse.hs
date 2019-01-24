module MathParse where

import Control.Monad (liftM2, foldM)
import Control.Arrow (left)
import Data.List (intersperse)
import Data.Bits

-- ======================== TYING IT ALL TOGETHER =============================

data Error = C ReduceError
           | P ParseError
    deriving (Show, Eq)

-- Entry point for calculation
-- Either 
-- * exits with an error, or 
-- * returns the integer from parsing the expression
calculate :: String -> Either Error Integer
calculate s = do
    e <- left P $ parseToExpr s
    i <- left C $ reduce e
    return i

-- Turns all errors into strings using printError before exiting
calculateStr :: String -> Either String Integer
calculateStr = left printError . calculate

-- Prints unified ReduceError and ParseError
printError :: Error -> String
printError (C x) = printReduceError x
printError (P x) = printParseError x

-- ======================== EXPRESSIONS MANIPULATION ==========================
data Expr = Num Integer
          | BinaryExpr Operator Expr Expr
    deriving (Eq, Show, Read)

data ReduceError = TooLarge
                 | NegativePower
    deriving (Show, Eq)

printReduceError :: ReduceError -> String
printReduceError TooLarge = "The expression entered was too large to be parsed."
printReduceError NegativePower = "A number was raised to a negative power, which can't be computed because MathParse only works on Integral numbers."

data Constraint = Constraint Operator Integer Integer

-- Reduces an Expression into a final integer, or exits with a ReduceError
reduce :: Expr -> Either ReduceError Integer
reduce (Num i)             = Right i
reduce (BinaryExpr op a b) = liftM2 (opToF op) a' b'
    where
    a' = reduce a
    b' = reduce b

-- Reduces, but sanitizes errors to strings
reduceStr :: Expr -> Either String Integer
reduceStr = left printReduceError . reduce

-- ============================ OPERATOR MANIPULATION =========================
data Operator = Equals
              | Or
              | Xor
              | And
              | Subtract
              | Add
              | Divide
              | Times
              | Exponentiate
              | OpenParen | CloseParen
    deriving (Eq, Enum, Ord, Bounded)

opToF :: Operator -> (Integer -> Integer -> Integer)
opToF Equals       = \x y -> toInteger $ fromEnum $ x == y
opToF Or           = (.|.)
opToF Xor          = xor
opToF And          = (.&.)
opToF Subtract     = (-)
opToF Add          = (+)
opToF Divide       = div
opToF Times        = (*)
opToF Exponentiate = (^)

enumerate :: (Bounded a, Enum a) => [a]
enumerate = [minBound .. maxBound]

rassoc, lassoc :: Operator -> Bool
rassoc Exponentiate = True
rassoc _            = False
lassoc = not . rassoc

prec :: Operator -> Int
prec = fromEnum

instance Show Operator where
    show CloseParen   = ")"
    show OpenParen    = "("
    show Exponentiate = "**"
    show Times        = "*"
    show Divide       = "/"
    show Add          = "+"
    show Subtract     = "-"
    show And          = "&"
    show Xor          = "^"
    show Or           = "|"
    show Equals       = "="

instance Read Operator where
    readsPrec _ s = [ (o,rest) 
                    | (s,rest) <- lex s
                    , o <- enumerate
                    , s == show o
                    ]

-- ================================== PARSING =================================

-- | Specialized reading utilities
-- Only read in positive integers
readsInt :: ReadS Integer
readsInt = filter ((>=0) . fst) . reads

-- Read in operations
readsOp :: ReadS Operator
readsOp = reads

data ParseError = UnusedOperators   [Operator]
                | UnusedOperands    [Expr]
                | NotEnoughOperators
                | NotEnoughOperands Operator
                | EmptyInput
                | UnidentifiedToken String
    deriving (Show, Eq)

printParseError :: ParseError -> String
printParseError (UnusedOperators ops) = "There were some unused operators: " ++ concat (intersperse ", " $ map show ops)
printParseError (UnusedOperands exprs) = "There were some unused operands: " ++ concat (intersperse ", " $ map show exprs)
printParseError (NotEnoughOperands op) = "There were not enough operands to satisfy the following operator: " ++ show op
printParseError NotEnoughOperators = "There were not enough operators to complete the expression."
printParseError EmptyInput = "Can't parse an empty input."
printParseError (UnidentifiedToken tok) = "Unidentified token: " ++ tok

-- Wrapper around shuntingYard
parseToExpr :: String -> Either ParseError Expr
parseToExpr = shuntingYard True [] []

-- Parses expressions, but changes errors to strings
parseToExprStr :: String -> Either String Expr
parseToExprStr = left printParseError . parseToExpr

-- Shunting Yard Algorithm
-- Actually parses strings into expressions
-- Look upon, ye mortals, and despair
shuntingYard :: Bool -> [Expr] -> [[Operator]] -> String -> Either ParseError Expr
shuntingYard unary []     []        "" = Left EmptyInput
shuntingYard unary []     operators "" = Left $ UnusedOperators $ concat operators
shuntingYard unary [expr] []        "" = Right $ expr
shuntingYard unary exprs  []        "" = Left $ UnusedOperands exprs
shuntingYard unary exprs  operators "" = head <$> applyAllOps exprs operators
shuntingYard unary exprs  operators input
    | not $ null $ readsInt input 
    = let (i,rest):_ = readsInt input
          exprs'     = Num i : exprs
       in shuntingYard False exprs' operators rest
    | not $ null $ readsOp input 
    = let (op,rest):_ = readsOp input
          ops' = insertOps operators op
       in if op == Subtract && unary == True
          then if not $ null $ readsInt rest
              then do
                  let (i,rest'):_ = readsInt rest
                  let exprs'      = Num (-i) : exprs
                  shuntingYard False exprs' operators rest'
              else Left $ NotEnoughOperands Subtract
          else if opNeedsPopping operators op
          then do
              (nextOps, remOps) <- extractNextOps ops'
              exprs' <- applyOps exprs nextOps
              shuntingYard True exprs' remOps rest
          else shuntingYard True exprs ops' rest
    | otherwise                   
    = Left $ UnidentifiedToken $ fst $ head $ lex input

insertOps :: [[Operator]] -> Operator -> [[Operator]]
insertOps [] newOp = [[newOp]]
insertOps ops OpenParen  = [OpenParen]  : ops
insertOps ops CloseParen = [CloseParen] : ops
insertOps ([OpenParen]:ops) op = [op] : [OpenParen] : ops
insertOps (op:ops) newOp 
    | newOp == head op && rassoc newOp && rassoc (head op) = (newOp:op):ops
    | newOp > head op = [newOp] : op : ops
    | otherwise  = op : insertOps ops newOp

opNeedsPopping :: [[Operator]] -> Operator -> Bool
opNeedsPopping [] _                       = False
opNeedsPopping ([OpenParen]:_) newOp      = False
opNeedsPopping _               CloseParen = True
opNeedsPopping ops newOp                  
  = let op = head $ head ops
     in or [ newOp < op 
           , and [ newOp == op 
                 , not $ rassoc newOp
                 , not $ rassoc op
                 ]
           ]

extractNextOps :: [[Operator]] -> Either ParseError ([Operator], [[Operator]])
extractNextOps []                  = Left NotEnoughOperators
extractNextOps xs@([CloseParen]:x) = extractWithinParen xs
extractNextOps (x:xs)              = Right (x,xs)

extractWithinParen :: [[Operator]] -> Either ParseError ([Operator], [[Operator]])
extractWithinParen ([CloseParen]:x:[OpenParen]:xs) 
  = Right $ (x,xs)
extractWithinParen ([CloseParen]:xs)               
  = Left $ UnusedOperators $ concat $ takeWhile (/=[CloseParen]) xs

applyAllOps :: [Expr] -> [[Operator]] -> Either ParseError [Expr]
applyAllOps = foldM applyOps

applyOps :: [Expr] -> [Operator] -> Either ParseError [Expr]
applyOps = foldM applyOp

applyOp :: [Expr] -> Operator -> Either ParseError [Expr]
applyOp = applyBinaryOp

applyBinaryOp :: [Expr] -> Operator -> Either ParseError [Expr]
applyBinaryOp (a:b:xs) op = Right $ BinaryExpr op a b : xs
applyBinaryOp _        op = Left $ NotEnoughOperands op
