module Expr where

import Expr.Core
import Expr.Reduce
import Expr.Parse

-- Error handling utility
import Control.Arrow (left)

-- Tagged union of Reduce and Parse errors
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
