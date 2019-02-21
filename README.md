# math-parser - The MathParse Haskell module

A simple module based on the shunting yard algorithm for parsing mathematical
expressions. It is capable of understanding arbitary binary operators,
considering right/left associativity and operator precedence.

# Usage
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
