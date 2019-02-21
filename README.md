# math-parser - The MathParse Haskell module

A simple module based on the shunting yard algorithm for parsing mathematical
expressions. It is capable of understanding arbitary binary and unary
operators, considering right/left associativity and operator precedence.

Currently, it only deals with integral mathematics.

# Usage
This package exports two root modules - Expr and MathParse. Please use Expr.
MathParse simply re-exports Expr and is around for legacy compatibility, please
do not use it.

Expr exports two pieces of machinery: Expr.Parse and Expr.Reduce. The former
parses ASTs from strings, and the latter turns those ASTs into a final integer.

Expr also exports `calculate` functions, which serve to glue together parsing
and reduction machinery.

`calculate` will take a string expression and Either throw an Error or
calculate an integer from it, e.g.
```hs
> calculate "1 * 3"
Right 3

> calculate "4 / 3"
Right 1

> calculate "1 / 0"
Left (C ZeroDivision)

> calculate ""
Left (P NoParses)
```

For more information about internals, read on!

## Expr.Parse
Parse turns input strings into ASTs of type `Expr`, e.g.  
`1` into `Num 1`  
`1 * 3` into `BinaryExpr Multiply (Num 1) (Num 3)`  

This can be done using the standard `Read` instance of `Expr`, or using
`parseToExpr`, which returns `Either` a `ParseError` or an `Expr`. `ParseError`
accounts for ambiguous parses, incomplete parses, and no parses, returning some
information on what kind of failure occurred, e.g.  
```hs
> read "1 * 3" :: Expr
BinaryExpr Multiply (Num 1) (Num 3)

> read "" :: Expr
*** Exception: Prelude.read: no parse

> parseToExpr "1 * 3"
Right (BinaryExpr Multiply (Num 1) (Num 3))

> parseToExpr "1 * 3 *"
Left (ParseEnd " *")

> parseToExpr ""
Left NoParses
```

All parsing respects operator precedence and applies suffix (postfix) operators
first, then prefix, then finally binary operators.

## Expr.Reduce
Reduce turns ASTs of type `Expr` into results of type `Integer`, e.g.  
`BinaryExpr Add (Num 2) (Num 3)` into `5`  
`BinaryExpr Divide (Num 7) (Num 2)` into `3`

The main reduction function `reduceWithConstraints` does this by taking a list
of constraints of type `Constraint` (explained later), then returning a *layer*
function which can reduce one layer of the tree, `Either` returning a
`ReduceError` or an `Integer`.
These *layer* functions thus have type:
`ExprF (Either ReduceError Integer) -> Either ReduceError Integer`. e.g.  
```hs
-- Don't pass in any constraints
> reduceWithConstraints [] 
it :: ExprF (Either ReduceError Integer) -> Either ReduceError Integer
```

By passing the layer function to `runReduce`, the layer function will
recursively be run over an entire AST.
`runReduce`.
```hs
> layerFunc = reduceWithConstraints []
> runReduce layerFunc (Num 2)
Right 2

> runReduce layerFunc (BinaryExpr Add (Num 2) (Num 3))
Right 5

> runReduce layerFunc (BinaryExpr Divide (Num 2) (Num 0))
*** Exception: divide by zero
```

**NOTE**: The `reduce` function is not suitable for `runReduce`. You can only
use its friends, `reduceWithConstraints` and `reduceSafe`, to produce layer
functions.

### Safer Reductions
In the last example, the layer function didn't protect against nasty issues like
division by zero. The `reduceSafe` function automatically provides two
constraints to protect against division by zero and negative exponents.
```hs
> runReduce (reduceSafe []) (BinaryExpr Add (Num 2) (Num 3))
Right 5

> runReduce (reduceSafe []) (BinaryExpr Divide (Num 2) (Num 0))
Left ZeroDivision

> runReduce (reduceSafe []) (BinaryExpr Exponentiate (Num 2) (Num -1))
Left NegativePower
```

### Constraints and How to Use Them
You can define limits for different operator behaviours using the 
`Constraint a` data type. Each constraint contains a pair of predicates and an
operator for which to apply the predicates, and a `ReduceError` to yield if both
predicates pass. E.g.  
```hs
> let myConstraint = Constraint (BinaryExprF Add (>2) (>2)) TooLarge
> runReduce (reduceWithConstraints [myConstraint]) (read "3 + 2")
Right 5

> runReduce (reduceWithConstraints [myConstraint]) (read "3 + 3")
Left TooLarge
```

There are three `ReduceErrors`: `TooLarge`, `NegativePower`, `ZeroDivision`

You can pass custom `Constraints` in to `reduceWithConstraints` or
`reduceSafe`, or even to `calculateSafe` and `calculateWithConstraints` inside
the root Expr module.

## Supported Operators
Currently, MathParse supports the following operators, in the following
precedence (highest to lowest):

| Text     | Un/Bin | Assoc  | Usage                                            |
|--------- |--------|--------|--------------------------------------------------|
|  !       | Unary  | Suffix | Factorial of operand                             |
|  ! , not | Unary  | Prefix | Bitwise complement of operand                    |
|  -       | Unary  | Prefix | Negate the given operand                         |
|  ** , ^  | Binary | Right  | Take first operand to the power of the second    |
|  *       | Binary | Left   | Multiply first operand by second                 |
|  /       | Binary | Left   | Divide first operand by second                   |
|  +       | Binary | Left   | Add first operand to second                      |
|  -       | Binary | Left   | Subtract second operand from first               |
|  &       | Binary | Left   | Bitwise and of both operands                     |
|  xor     | Binary | Left   | Bitwise xor of both operands                     |
| \|       | Binary | Left   | Bitwise or of both operands                      |
|  =       | Binary | Left   | Outputs 1 if the operands are equal, 0 otherwise |

Implementing more operators is trivial, so please feel free to ask in an issue
if you would like any new operator specified, or feel free to write it yourself
and submit a pull request!

# Algorithmic Underpinnings
Uses parser combinators from the parser-combinators library to understand
expressions and turn them into a syntax tree. The previous implementation,
which used the shunting yard algorithm, proved unwieldy for postfix/prefix
operators.

~Refer to https://www.engr.mun.ca/~theo/Misc/exp_parsing.htm for the explanation
I referenced.~
