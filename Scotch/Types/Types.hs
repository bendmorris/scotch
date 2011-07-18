{-  This file is part of Scotch.

    Scotch is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Scotch is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Scotch.  If not, see <http://www.gnu.org/licenses/>.
-}

module Scotch.Types.Types where


upperCase = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
lowerCase = "abcdefghijklmnopqrstuvwxyz"
numeric = "0123456789"
idSymbol = "_!'."
operatorSymbol = "!@#$%^&*+-*/=<>?|`"
forbiddenOps = ["+", "-", "*", "/", "^", "=", ":=", "==",
                "!=", "<", ">", "and", "or", "not", ":", 
                "<=", ">=", "+=", "<<", ">>", "..", "::",
                "@", "mod", "%", "->", "<-", "=>",
                "+=", "-=", "*=", "/=", "^=", "%=",
                "&", "|"
                ]

                     
-- binds a left term to a right term
type Binding = (Expr, Expr)

-- a value identifier
type Id = String
-- a value with its corresponding type
data Value = NumInt Integer
           | NumFloat Double
           | Str String
           | Bit Bool
           | Hash [[(String, Expr)]]
           | Null
           | Lambda [Id] Expr
           | Proc [Expr]
           | Thread Expr
           | Undefined String
           | File String
           | InvalidValue
           deriving Eq

-- represents an arithmetic expression
data Expr = Exception String                -- undefined
          | Skip                            -- returns Null
          | Val (Value)                     -- value
          | List [Expr]                     -- list expression
          | Take Expr Expr                  -- take _ from _
          | HashExpr [(Expr, Expr)]         -- hash expression
          | Subs Expr Expr                  -- list subscript
          | Concat Expr Expr                -- list concatenation
          | Add Expr Expr                   -- addition
          | Sub Expr Expr                   -- subtraction
          | Prod Expr Expr                  -- product
          | Neg Expr                        -- negation
          | Div Expr Expr                   -- division
          | Mod Expr Expr                   -- find remainder
          | Exp Expr Expr                   -- exponent
          | Eq Expr Expr                    -- equality
          | InEq Expr Expr                  -- inequality
          | Gt Expr Expr                    -- greater than
          | Lt Expr Expr                    -- less than
          | And Expr Expr                   -- boolean and
          | Or Expr Expr                    -- boolean or
          | Not Expr                        -- boolean not
          | Def Expr Expr Expr              -- rewriting rule assignment
          | EagerDef Expr Expr Expr         -- eager assignment
          | Var Id                          -- identifier with optional list of values
          | Call Expr [Expr]                -- call an expression representing a function
          | If Expr Expr Expr               -- conditional
          | Case Expr [(Expr, Expr)]        -- case expression
          | For Id (Expr) (Expr) [Expr]     -- iteration
          | TakeFor Id (Expr) (Expr) [Expr] Integer
                                            -- take from list comprehension
          | Range (Expr) (Expr) (Expr)      -- range
          | Input                           -- get a line of input from the user
          | Import [String] [String]        -- import module
          | FileObj Expr                    -- file object
          | FileRead Expr                   -- read file
          | FileWrite Expr Expr             -- write to file
          | FileAppend Expr Expr            -- append to file
          | EvalExpr Expr                   -- eval for metaprogramming
          | Rule [Expr]                     -- a rule (list of definitions)
          | UseRule (Expr) (Expr)           -- evaluate using a rule
          deriving Eq

type ExprPosition = (String, (Int, Int))
type PosExpr = (Maybe ExprPosition, Expr)
