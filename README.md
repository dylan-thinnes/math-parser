# math-parser - The MathParse Haskell module

A simple module based on the shunting yard algorithm for parsing mathematical
expressions. It is capable of understanding arbitary binary operators,
considering right/left associativity and operator precedence.

# Usage
Currently, MathParse supports the following binary operators, in the following
precedence (highest to lowest):

| Text | Assoc | Usage                                            |
|------|-------|--------------------------------------------------|
|  **  | Right | Take first operand to the power of the second    |
|  *   | Left  | Multiply first operand by second                 |
|  /   | Left  | Divide first operand by second                   |
|  +   | Left  | Add first operand to second                      |
|  -   | Left  | Subtract second operand from first               |
|  &   | Left  | Bitwise and of both operands                     |
|  ^   | Left  | Bitwise xor of both operands                     |
|  |   | Left  | Bitwise or of both operands                      |
|  =   | Left  | Outputs 1 if the operands are equal, 0 otherwise |

Implementing more binary operators is trivial, so please feel free to ask in an
issue if you would like any new operator specified, or feel free to write it
yourself and submit a pull request!

# Limits
Attempting to take exponents of numbers that are too large will result in a
TooLarge error being thrown. This is because MathExpr was originally designed
to be part of a front-facing API.

Removing these limits are trivial - please tell me if you would like them
removed.

# Algorithmic Underpinnings
Refer to https://www.engr.mun.ca/~theo/Misc/exp_parsing.htm for the explanation
I referenced.

The explanation does not go into how right-associative operators, unary
negation, and parentheses would be parsed, so I figured them out. Please ask in
an issue if you would like me to write about them.
