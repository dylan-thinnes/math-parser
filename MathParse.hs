module MathParse where

import Control.Monad (liftM2)

-- ======================== EXPRESSIONS MANIPULATION ==========================
data Expr = Num Integer
          | Expr :**: Expr
          | Expr :*: Expr
          | Expr :/: Expr
          | Expr :+: Expr
          | Expr :-: Expr
    deriving (Eq, Show, Read)

data CalcError = TooLarge
               | NegativePower
    deriving (Show, Eq)

calculate :: Expr -> Either CalcError Integer
calculate (Num i)   = Right i
calculate (a :**: b) = do
    a' <- calculate a
    b' <- calculate b
    if or [ b' > 1000000
          , a' > 100 && b' > 100000
          ] 
    then Left TooLarge
    else if b' < 0 
    then Left NegativePower
    else liftM2 (^) (calculate a) (calculate b)
calculate (a :*: b) = liftM2 (*) (calculate a) (calculate b)
calculate (a :/: b) = liftM2 div (calculate a) (calculate b)
calculate (a :+: b) = liftM2 (+) (calculate a) (calculate b)
calculate (a :-: b) = liftM2 (-) (calculate a) (calculate b)

-- ============================ OPERATOR MANIPULATION =========================
data Operator = Subtract
              | Add
              | Divide
              | Times
              | Exponentiate
              | OpenParen | CloseParen
    deriving (Eq, Enum, Ord, Bounded)

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
