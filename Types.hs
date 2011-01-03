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

module Types where

import Text.ParserCombinators.Parsec

-- a bindable identifier
data Id = Name String 
        | Pattern Value 
        | Split String String 
        | AtomMatch String [Id]
        deriving Eq
instance Show(Id) where
    show (Name s) = s
    show (Pattern v) = show v
    show (Split a b) = "(" ++ show a ++ " : " ++ show b ++ ")"
    show (AtomMatch a b) = show a ++ " " ++ show b

-- a value with its corresponding type
data Value = NumInt Integer
           | NumFloat Double
           | Str String
           | Bit Bool
           | List [Value]
           | Null
           | HFunc Id
           | Lambda [Id] Expr
           | Proc [Expr]
           | Thread Expr
           | Undefined String
           | File String
           | Atom String [Value]
           deriving Eq
instance Show (Value) where
    show (Str s) = "\"" ++ s ++ "\""
    show (NumInt n) = show n
    show (NumFloat n) = show n
    show (Bit True) = "true"
    show (Bit False) = "false"
    show (List l) = show l
    show (HFunc f) = "func " ++ show f
    show (Lambda ids expr) = "lambda " ++ show ids ++ " " ++ show expr
    show (Proc p) = "proc " ++ show p
    show (Thread th) = "thread " ++ show th
    show (Null) = "null"
    show (Undefined s) = show s
    show (File f) = "<<" ++ show f ++ ">>"
    show (Atom s v) = s ++ (case length v of
                              0 -> ""
                              1 -> " " ++ show (v !! 0)
                              otherwise -> " " ++ show v)

-- represents an arithmetic expression
data Expr = Exception String                -- undefined
          | Skip                            -- returns Null
          | Val (Value)                     -- value
          | ListExpr [Expr]                 -- list expression
          | ToInt (Expr)                    -- conversion to integer
          | ToFloat (Expr)                  -- conversion to float
          | ToStr (Expr)                    -- conversion to string
          | Subs Expr Expr                  -- list subscript
          | Add Expr Expr                   -- addition
          | Sub Expr Expr                   -- subtraction
          | Prod Expr Expr                  -- product
          | Neg Expr                        -- negation
          | Div Expr Expr                   -- division
          | Exp Expr Expr                   -- exponent
          | Eq Expr Expr                    -- equality
          | InEq Expr Expr                  -- inequality
          | Gt Expr Expr                    -- greater than
          | Lt Expr Expr                    -- less than
          | And Expr Expr                   -- boolean and
          | Or Expr Expr                    -- boolean or
          | Not Expr                        -- boolean not
          | Def Id Expr Expr                -- identifier assignment
          | EagerDef Id Expr Expr           -- identifier assignment
          | Defun Id [Id] Expr Expr         -- function definition
          | Defproc Id [Id] [Expr] Expr     -- procedure definition
          | Var Id                          -- identifier
          | Func Id [Expr]                  -- function call
          | LambdaCall Value [Expr]         -- lambda function call
          | If Expr Expr Expr               -- conditional
          | Case Expr [(Id, Expr)]          -- case expression
          | For Id (Expr) (Expr)            -- iteration
          | Range (Expr) (Expr) (Expr)      -- range
          | Output Expr                     -- output
          | Input                           -- get a line of input from the user
          | Import [String]                 -- import module
          | FileObj Expr                    -- file object
          | FileRead Expr                   -- read file
          | FileWrite Expr Expr             -- write to file
          | FileAppend Expr Expr            -- append to file
          | AtomExpr String [Expr]          -- evaluates to an atom value
          deriving Eq
instance Show(Expr) where
    show (Exception s) = "Exception: " ++ s
    show Skip = "*nothing*"
    show (Val v) = show v
    show (ListExpr l) = show l
    show (ToInt i) = "int(" ++ show i ++ ")"
    show (ToFloat f) = "float(" ++ show f ++ ")"
    show (ToStr s) = "str(" ++ show s ++ ")"
    show (Subs n s) = show s ++ "[" ++ show n ++ "]"
    show (Add x y) = show x ++ " + " ++ show y
    show (Sub x y) = show x ++ " - " ++ show y
    show (Prod x y) = show x ++ " * " ++ show y
    show (Div x y) = show x ++ " / " ++ show y
    show (Exp x y) = show x ++ " ^ " ++ show y
    show (Eq x y) = show x ++ " == " ++ show y
    show (Gt x y) = show x ++ " > " ++ show y
    show (Lt x y) = show x ++ " < " ++ show y
    show (And x y) = show x ++ " & " ++ show y
    show (Or x y) = show x ++ " | " ++ show y
    show (Not x) = "not " ++ show x
    show (Def a b c) = "(def " ++ show a ++ " as " ++ show b ++ ", " ++ show c ++ ")"
    show (EagerDef a b c) = "(eager def " ++ show a ++ " as " ++ show b ++ ", " ++ show c ++ ")"
    show (Defun a b c d) = "(defun " ++ (show a) ++ " " ++ (show b) ++ " as " ++ show c ++ ", " ++ show d ++ ")"
    show (Defproc a b c d) = "(defproc " ++ (show a) ++ " " ++ (show b) ++ " as {" ++ (show c) ++ "}, " ++ (show d) ++ ")"
    show (Var v) = "var " ++ show v
    show (Func f p) = "(func " ++ (show f) ++ " " ++ (show p) ++ ")"
    show (If cond x y) = "(if " ++ show cond ++ " then " ++ show x ++ " else " ++ show y ++ ")"
    show (Case c o) = "(case " ++ show c ++ " " ++ show o ++ ")"
    show (For x y z) = "[for " ++ (show x) ++ " in " ++ (show y) ++ " " ++ (show z) ++ "]"
    show (Range x y z) = "range(" ++ (show x) ++ "," ++ (show y) ++ "," ++ (show z) ++ ")"
    show (Output x) = "print " ++ show x
    show Input = "input"
    show (Import s) = "import " ++ (show s)
    show (FileObj f) = "<<" ++ show f ++ ">>"
    show (FileRead f) = "read " ++ show f
    show (FileWrite f x) = "write " ++ show f ++ " " ++ show x
    show (FileAppend f x) = "append " ++ show f ++ " " ++ show x
    show (AtomExpr s x) = "(" ++ s ++ " " ++ show x ++ ")"
type PosExpr = (Maybe SourcePos, Expr)
