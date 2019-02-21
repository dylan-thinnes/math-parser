module Expr.Reduce
    ( Constraint(..)
    , ReduceError(..)
    , runReduce
    , reduce, reduceSafe, reduceWithConstraints
    , negativePower, zeroDivision
    , binOpToF, unOpToF
    ) where

import Expr.Core
import Data.Bits
import Data.Functor.Foldable (cata)
import Data.Maybe (mapMaybe, listToMaybe)

data ReduceError = TooLarge
                 | NegativePower
                 | ZeroDivision
    deriving (Eq)

type Reduction = Either ReduceError Integer

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
    | (UnaryExprF predOp predA) <- conds
    , (UnaryExprF op     a    ) <- expression
    , predOp == op && predA a
    = Just err
    | otherwise
    = Nothing

checkAll :: [Constraint a] -> ExprF a -> Maybe ReduceError
checkAll constraints expression 
  = listToMaybe $ mapMaybe (flip check expression) constraints

-- Executes a reduction as a catamorphism
runReduce :: (ExprF Reduction -> Reduction) -> Expr -> Reduction
runReduce = cata

-- Reduces a single layer expression into an integer
reduce :: ExprF Integer -> Integer
reduce (NumF i) = i
reduce (BinaryExprF op a b) = binOpToF op a b
reduce (UnaryExprF op a) = unOpToF op a

-- Reduces a single layer expression into an integer, with constraints,
-- automatically protects against negative exponents / zero division
reduceSafe :: [Constraint Integer] -> ExprF Reduction -> Reduction
reduceSafe cs = reduceWithConstraints (zeroDivision:negativePower:cs)

-- Constraint for checking if num being taken to a negative power
negativePower :: Constraint Integer
negativePower = Constraint 
    { conds = BinaryExprF Exponentiate (const True) (<0)
    , err   = NegativePower
    }

-- Constraint for checking if division by zero
zeroDivision :: Constraint Integer
zeroDivision = Constraint 
    { conds = BinaryExprF Divide (const True) (==0)
    , err   = ZeroDivision
    }

-- Reduces a single layer expression into an integer, with constraints
reduceWithConstraints :: [Constraint Integer] -> ExprF Reduction -> Reduction
reduceWithConstraints constraints (NumF i) = return $ reduce (NumF i)
reduceWithConstraints constraints (UnaryExprF op a) = do
    fstExpr <- a
    let pureExpr = UnaryExprF op fstExpr
    case checkAll constraints pureExpr of
      Nothing  -> return $ reduce pureExpr
      Just err -> Left err
reduceWithConstraints constraints (BinaryExprF op a b) = do
    fstExpr <- a
    sndExpr <- b
    let pureExpr = BinaryExprF op fstExpr sndExpr
    case checkAll constraints pureExpr of
      Nothing  -> return $ reduce pureExpr
      Just err -> Left err

-- Operations for BinOp
binOpToF :: BinOp -> (Integer -> Integer -> Integer)
binOpToF Exponentiate = (^)
binOpToF Multiply     = (*)
binOpToF Divide       = div
binOpToF Add          = (+)
binOpToF Subtract     = (-)
binOpToF And          = (.&.)
binOpToF Xor          = xor
binOpToF Or           = (.|.)
binOpToF Equals       = \x y -> toInteger $ fromEnum $ x == y

-- Operations for UnOp
unOpToF :: UnOp -> (Integer -> Integer)
unOpToF Negate    = negate
unOpToF Not       = complement
unOpToF Factorial = \n -> product [1..n]

