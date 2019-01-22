module MathParse where

data Expr = Num Integer
          | Expr :**: Expr
          | Expr :*: Expr
          | Expr :/: Expr
          | Expr :+: Expr
          | Expr :-: Expr
    deriving (Eq, Show, Read)

data Operator = Subtract
              | Add
              | Divide
              | Times
              | Exponentiate
              | OpenParen | CloseParen
    deriving (Eq, Enum, Ord, Bounded)
