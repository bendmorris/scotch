module Types where

import Data.Decimal
import Text.ParserCombinators.Parsec

-- a bindable identifier
data Id = Name String | Pattern Value | Split String String deriving Eq
instance Show(Id) where
    show (Name s) = s
    show (Pattern v) = show v
    show (Split a b) = "(" ++ show a ++ " : " ++ show b ++ ")"
-- a list of identifiers (empty list for variables) and an expression containing them
type Call = ([Id], Expr)
-- binds an ID to a Call
type Binding = (Id, Call)
-- binding with associated scope (amount of whitespace)
type ScopedBinding = (Int, Binding)
-- given a scoped binding, returns an unscoped binding
unscope :: [ScopedBinding] -> [Binding]
unscope [] = []
unscope (h:t) = (snd h) : unscope t


-- a value with its corresponding type
data Value = NumInt Integer
           | NumFloat Double
           | NumDec Decimal
           | Str String
           | Bit Bool
           | List [Value]
           | Null
           | HFunc Id
           | Proc [Expr]
           | UndefinedValue String
           | File String
           deriving Eq
instance Show (Value) where
    show (Str s) = "\"" ++ s ++ "\""
    show (NumInt n) = show n
    show (NumFloat n) = show n
    show (NumDec n) = show n
    show (Bit True) = "true"
    show (Bit False) = "false"
    show (List l) = show l
    show (HFunc f) = "func " ++ show f
    show (Proc p) = "proc " ++ show p
    show (Null) = "null"
    show (UndefinedValue s) = show s
    show (File f) = "<<" ++ show f ++ ">>"

-- a calculation, which may cause an exception if one of its members contains an exception
data Calculation = Exception String
                 | Result (Value) 
                 | Incomplete (Expr) 
                 | PrintOutput String
                 | FileOutput String String
                   deriving Eq
instance Show (Calculation) where
    show (Result r) = show r
    show (Exception s) = "Exception: " ++ s
    show (Incomplete e) = show e
    show (PrintOutput p) = show p
    show (FileOutput f x) = show f ++ " " ++ show x

-- represents an arithmetic expression
data Expr = 
            Undefined String                -- undefined
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
          | If Expr Expr Expr               -- conditional
          | For Id (Expr) (Expr)            -- iteration
          | Range (Expr) (Expr) (Expr)      -- range
          | Output Expr                     -- output
          | Placeholder                     -- the next statement should go here
          | Import [String]                 -- import module
          | FileObj Expr                    -- file object
          | FileRead Expr                   -- read file
          | FileWrite Expr Expr             -- write to file
          deriving Eq
se' :: (Show a) => [a] -> String
se' [] = []
se' (h:t) = " " ++ show h ++ (se' t)
se :: (Show a) => String -> [a] -> String
se sym (h:t) = "(" ++ sym ++ " " ++ (show h) ++ (se' t) ++ ")"
instance Show(Expr) where
    show (Undefined s) = s
    show Skip = "Skip"
    show (Val v) = show v
    show (ListExpr l) = show l
    show (ToInt i) = "int(" ++ show i ++ ")"
    show (ToFloat f) = "float(" ++ show f ++ ")"
    show (ToStr s) = "str(" ++ show s ++ ")"
    show (Subs n s) = show s ++ "[" ++ show n ++ "]"
    show (Add x y) = se "+" [x, y]
    show (Sub x y) = se "-" [x, y]
    show (Prod x y) = se "*" [x, y]
    show (Div x y) = se "/" [x, y]
    show (Exp x y) = se "^" [x, y]
    show (Eq x y) = se "==" [x, y]
    show (Gt x y) = se ">" [x, y]
    show (Lt x y) = se "<" [x, y]
    show (And x y) = se "&" [x, y]
    show (Or x y) = se "|" [x, y]
    show (Not x) = se "!" [x]
    show (Def a b c) = "(def " ++ (show a) ++ se' [b, c] ++ ")"
    show (EagerDef a b c) = "(eager def " ++ (show a) ++ se' [b, c] ++ ")"
    show (Defun a b c d) = "(defun " ++ (show a) ++ " " ++ (show b) ++ se' [c, d] ++ ")"
    show (Defproc a b c d) = "(defproc " ++ (show a) ++ " " ++ (show b) ++ "{" ++ (show c) ++ "}" ++ (show d) ++ ")"
    show (Var v) = "var " ++ show v
    show (Func f p) = "(func " ++ (show f) ++ " " ++ (show p) ++ ")"
    show (If cond x y) = se "if" [cond, x, y]
    show (For x y z) = "(for " ++ (show x) ++ " in " ++ (show y) ++ " " ++ (show z) ++ ")"
    show (Range x y z) = "range(" ++ (show x) ++ "," ++ (show y) ++ "," ++ (show z) ++ ")"
    show (Output x) = "print " ++ show x
    show (Placeholder) = "**nothing**"
    show (Import s) = "import " ++ (show s)
    show (FileObj f) = "<<" ++ show f ++ ">>"
    show (FileRead f) = "read " ++ show f
    show (FileWrite f x) = "write " ++ show f ++ " " ++ show x
    
type PosExpr = (Maybe SourcePos, Expr)
