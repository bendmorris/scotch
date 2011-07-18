module Scotch.Types.Show where

import Data.List
import Data.List.Utils
import Numeric
import Scotch.Types.Types


formatString [] = []
formatString (h:t) = if h == '"' then "\\\"" ++ formatString t
                     else h : formatString t
removeBrackets s = if (s !! 0) == '[' && (s !! (l-1)) == ']'
                   then "(" ++ [s !! n | n <- [1..l-2]] ++ ")"
                   else s
                   where l = length s
moduleName s = join "." s


instance Show (Value) where
    show (Str s) = "\"" ++ formatString s ++ "\""
    show (NumInt n) = show n
    show (NumFloat n) = showFFloat Nothing n ""
    show (Bit True) = "true"
    show (Bit False) = "false"
    show (Hash h) = "{" ++ (if length h > 0
                            then join ", " ["\"" ++ fst i ++ "\": " ++ show (snd i) | j <- h, i <- j]
                            else "") ++ "}"
    show (Lambda ids expr) = "(" ++ (join ", " ids) ++ ") -> " ++ show expr
    show (Proc p) = join ", " [show i | i <- p]
    show (Thread th) = "thread " ++ show th
    show (Null) = "null"
    show (Undefined s) = show s
    show (File f) = "file(" ++ show f ++ ")"
    show InvalidValue = "**invalid value**"


instance Show(Expr) where
    show (Exception s) = "Exception: " ++ s
    show Skip = ""
    show (Val v) = show v
    show (List l) = show l
    show (Take a b) = "take " ++ show a ++ " from " ++ show b
    show (TakeFor a b c d e) = show (Take (Val (NumInt e)) (For a b c d))
    show (HashExpr h) = "{" ++ (if length h > 0
                                then join ", " [show (fst i) ++ ": " ++ show (snd i) | i <- h]
                                else "") ++ "}"
    show (Concat a b) = "(" ++ show a ++ " : " ++ show b ++ ")"
    show (Subs n s) = show s ++ " @" ++ show n
    show (Add x y) = "(" ++ show x ++ " + " ++ show y ++ ")"
    show (Sub x y) = "(" ++ show x ++ " - " ++ show y ++ ")"
    show (Prod (Val (NumInt x)) (Var y)) = show x ++ y
    show (Prod (Val (NumInt x)) y) = show x ++ if show y !! 0 == '(' then show y else "(" ++ show y ++ ")"
    show (Prod (Val (NumFloat x)) (Val y)) = show x ++ if show y !! 0 == '(' then show y else "(" ++ show y ++ ")"
    show (Prod x y) = "(" ++ show x ++ " * " ++ show y ++ ")"
    show (Div x y) = "(" ++ show x ++ " / " ++ show y ++ ")"
    show (Mod x y) = "(" ++ show x ++ " mod " ++ show y ++ ")"
    show (Exp x y) = "(" ++ show x ++ " ^ " ++ show y ++ ")"
    show (Eq x y) = "(" ++ show x ++ " == " ++ show y ++ ")"
    show (InEq x y) = "(" ++ show x ++ " != " ++ show y ++ ")"
    show (Gt x y) = "(" ++ show x ++ " > " ++ show y ++ ")"
    show (Lt x y) = "(" ++ show x ++ " < " ++ show y ++ ")"
    show (And x y) = "(" ++ show x ++ " & " ++ show y ++ ")"
    show (Or x y) = "(" ++ show x ++ " | " ++ show y ++ ")"
    show (Not x) = "not " ++ show x
    show (Def a (Rule r) Skip) = "rule " ++ show a ++ " =" ++ show (Rule r)
    show (Rule r) = join ", " [show i | i <- r]
    show (Def a b Skip) = show a ++ " = " ++ show b
    show (Def a b c) = "(" ++ show c ++ " where " ++ show a ++ " = " ++ show b ++ ")"
    show (EagerDef a b Skip) = show a ++ " := " ++ show b
    show (EagerDef a b c) = "(" ++ show c ++ " where " ++ show a ++ " := " ++ show b ++ ")"
    show (UseRule r x) = "using " ++ show r ++ " => " ++ show x
    show (Var f) = f
    show (Call f args) = show f ++ removeBrackets (show args)
    show (If (Call (Var "bool") [cond]) x y) = "if " ++ show cond ++ " then " ++ show x ++ " else " ++ show y
    show (If cond x y) = "if " ++ show cond ++ " then " ++ show x ++ " else " ++ show y
    show (Case c o) = "case " ++ show c ++ " of" ++ tail (foldl (++) "" [", " ++ show (fst i) ++ " -> " ++ show (snd i) | i <- o])
    show (For x (Call (Var "list") [y]) z w) = "[for " ++ show x ++ " in " ++ show y ++ ", " ++ show z ++ (foldl (++) "" [", " ++ show w' | w' <- w]) ++ "]"
    show (For x y z w) = "[for " ++ show x ++ " in " ++ show y ++ ", " ++ show z ++ (foldl (++) "" [", " ++ show w' | w' <- w]) ++ "]"
    show (Range x y z) = "[" ++ show x ++ ".." ++ show y ++ (if z == (Val (NumInt 1)) then "" else "," ++ show z) ++ "]"
    show Input = "input"
    show (Import s []) = "import " ++ moduleName s
    show (Import s t) = show (Import s []) ++ " as " ++ moduleName t
    show (FileObj f) = "file(" ++ show f ++ ")"
    show (FileRead f) = "read(" ++ show f ++ ")"
    show (FileWrite f x) = "write(" ++ show f ++ ", " ++ show x ++ ")"
    show (FileAppend f x) = "append(" ++ show f ++ ", " ++ show x ++ ")"
    show (EvalExpr e) = "eval(" ++ show e ++ ")"
