module MathParse where

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

data ParseError = UnusedOperators   [Operator]
                | UnusedOperands    [Expr]
                | NotEnoughOperators
                | NotEnoughOperands Operator
                | EmptyInput
                | UnidentifiedToken String
    deriving (Show, Eq)
