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
