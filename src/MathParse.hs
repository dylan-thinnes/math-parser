{-# LANGUAGE DeriveFunctor, TypeFamilies, InstanceSigs #-}
module MathParse where

import Control.Monad (liftM2, foldM)
import Control.Arrow (left)
import Data.List (intersperse)
import Data.Bits
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Functor.Foldable

-- ======================== TYING IT ALL TOGETHER =============================

data Error = C ReduceError
           | P ParseError
    deriving (Eq)

-- Entry point for calculation
-- Either 
-- * exits with an error, or 
-- * returns the integer from parsing the expression
calculate :: String -> Either Error Integer
calculate = calculateSafe []

calculateSafe :: [Constraint Integer] -> String -> Either Error Integer
calculateSafe constraints s = do
    e <- left P (parseToExpr s)
    left C $ runReduce (reduceSafe constraints) e

calculateWithConstraints :: [Constraint Integer] -> String -> Either Error Integer
calculateWithConstraints constraints s = do
    e <- left P (parseToExpr s)
    left C $ runReduce (reduceWithConstraints constraints) e

-- Prints unified ReduceError and ParseError
instance Show Error where
    show (C x) = show x
    show (P x) = show x

-- ======================== EXPRESSIONS MANIPULATION ==========================
data Expr = Num Integer
          | BinaryExpr Operator Expr Expr
    deriving (Eq, Read)

data ExprF a = NumF Integer
             | BinaryExprF Operator a a
    deriving Functor

type instance Base Expr = ExprF

instance Recursive Expr where
    project :: Expr -> ExprF Expr
    project (Num i) = NumF i
    project (BinaryExpr op a b) = BinaryExprF op a b

instance Corecursive Expr where
    embed :: ExprF Expr -> Expr
    embed (NumF i) = Num i
    embed (BinaryExprF op a b) = BinaryExpr op a b

instance Show Expr where
    show (Num i) = show i
    show (BinaryExpr op a b) = "(" 
                            ++ show a 
                            ++ " " 
                            ++ show op 
                            ++ " " 
                            ++ show b 
                            ++ ")"

data ReduceError = TooLarge
                 | NegativePower
                 | ZeroDivision
    deriving (Eq)

instance Show ReduceError where
    show TooLarge = "The expression entered was too large to be parsed."
    show NegativePower = "A number was raised to a negative power, which can't be computed because MathParse only works on Integral numbers."
    show ZeroDivision = "A number was divided by zero, which can't be computed as division by zero is undefined."

data Constraint a = Constraint
    { conds :: ExprF (a -> Bool)
    , err   :: ReduceError
    }

check :: Constraint a -> ExprF a -> Maybe ReduceError
check (Constraint conds err) expression
    | (BinaryExprF predOp predA predB) <- conds
    , (BinaryExprF op     a     b    ) <- expression
    , predOp == op && predA a && predB b
    = Just err
    | otherwise
    = Nothing

checkAll :: [Constraint a] -> ExprF a -> Maybe ReduceError
checkAll constraints expression 
  = listToMaybe $ mapMaybe (flip check expression) constraints

-- Executes a reduction as a catamorphism
runReduce = cata

-- Reduces a single layer expression into an integer
reduce :: ExprF Integer -> Integer
reduce (NumF i) = i
reduce (BinaryExprF op a b) = opToF op a b

-- Reduces a single layer expression into an integer, with constraints,
-- automatically protects against negative exponents
reduceSafe :: [Constraint Integer] -> ExprF (Either ReduceError Integer) -> Either ReduceError Integer
reduceSafe cs = reduceWithConstraints (zeroDivision:negativePower:cs)

negativePower :: Constraint Integer
negativePower = Constraint 
    { conds = BinaryExprF Exponentiate (const True) (<0)
    , err   = NegativePower
    }

zeroDivision :: Constraint Integer
zeroDivision = Constraint 
    { conds = BinaryExprF Divide (const True) (==0)
    , err   = ZeroDivision
    }

reduceWithConstraints :: [Constraint Integer] -> ExprF (Either ReduceError Integer) -> Either ReduceError Integer
reduceWithConstraints constraints (NumF i) = return $ reduce (NumF i)
reduceWithConstraints constraints (BinaryExprF op a b) = do
    fstExpr <- a
    sndExpr <- b
    return $ reduce (BinaryExprF op fstExpr sndExpr)

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
    deriving (Eq)

instance Show ParseError where
    show (UnusedOperators ops) = "There were some unused operators: " ++ concat (intersperse ", " $ map show ops)
    show (UnusedOperands exprs) = "There were some unused operands: " ++ concat (intersperse ", " $ map show exprs)
    show (NotEnoughOperands op) = "There were not enough operands to satisfy the following operator: " ++ show op
    show NotEnoughOperators = "There were not enough operators to complete the expression."
    show EmptyInput = "Can't parse an empty input."
    show (UnidentifiedToken tok) = "Unidentified token: " ++ tok

-- Wrapper around shuntingYard
parseToExpr :: String -> Either ParseError Expr
parseToExpr = shuntingYard True [] []

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
applyBinaryOp (a:b:xs) op = Right $ BinaryExpr op b a : xs
applyBinaryOp _        op = Left $ NotEnoughOperands op
