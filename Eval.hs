module Eval where

import Types
import Calc

-- evalList: checks a list for exceptions
evalList [] = Result (Bit True)
evalList (h:t) = case h of
                   Val r -> evalList t
                   Undefined e -> Exception e
                   otherwise -> Exception "Non-value used in list."

-- eval: computes the result of an expression as a Calculation
weval :: Expr -> [Binding] -> Calculation
weval exp vars = case exp of
                    Import s -> Incomplete (Placeholder)
                    Undefined s -> Exception ("Undefined: " ++ s)
                    Placeholder -> Incomplete (Placeholder)
                    Skip -> Result Null
                    Val (List l) -> case (evalList r) of
                                        Result _ -> Result (List r)
                                        Exception e -> Exception e
                                    where r = [(case (weval item vars) of
                                                  Result r -> Val r
                                                  Exception e -> Undefined e
                                                  Incomplete i -> i
                                                ) | item <- l]
                    Val x -> Result x
                    Add x y -> calc (weval x vars) (weval y vars) (vadd)
                    Sub x y -> calc (weval x vars) (weval y vars) (vsub)
                    Prod x y -> calc (weval x vars) (weval y vars) (vprod)
                    Div x y -> calc (weval x vars) (weval y vars) (vdiv)
                    Exp x y -> calc (weval x vars) (weval y vars) (vexp)
                    Eq x y -> calc (weval x vars) (weval y vars) (veq)
                    InEq x y -> case calc (weval x vars) (weval y vars) (veq) of
                                Result (Bit True) -> Result (Bit False)
                                Result (Bit False) -> Result (Bit True)
                                otherwise -> otherwise                    
                    Gt x y -> calc (weval x vars) (weval y vars) (vgt)
                    Lt x y -> calc (weval x vars) (weval y vars) (vlt)                    
                    And x y -> calc (weval x vars) (weval y vars) (vand)
                    Or x y -> calc (weval x vars) (weval y vars) (vor)
                    Not x -> case weval x vars of
                                Exception s -> Exception s
                                Result r -> case r of
                                                Bit b -> Result (Bit (not b))
                                                otherwise -> Exception "Expected boolean"
                    Def id x y -> weval y ((id, ([], x)) : vars)
                    EagerDef id x y -> weval y ((id, ([], eager_eval x vars)) : vars)
                    Defun id params x y -> weval y ((id, (params, x)) : vars)
                    Var x -> weval (snd definition) vars
                             where definition = var_binding x vars
                    Func f args -> if length params > 0 
                                   then weval (funcall (expr)
                                               (zip params args) vars) vars
                                   else case snd $ var_binding f vars of
                                          Var (Name f') -> weval (Func (Name f') args) vars
                                          Func f' args' -> weval (Func f' (args' ++ args)) vars
                                          otherwise -> Exception $ "Function " ++ (show f) ++ " doesn't match any existing pattern."
                                   where definition = func_binding f args vars
                                         params = fst definition
                                         expr = snd definition
                    If cond x y -> case (weval cond vars) of
                                     Result (Bit True) -> weval x vars
                                     Result (Bit False) -> weval y vars
                                     Exception e -> Exception e
                                     otherwise -> Exception $ "Non-boolean condition " ++ show cond
                    For id x y -> weval (case (weval x vars) of
                                            Result (List l) -> Val (List (forloop id l y))
                                            Result v -> Val (List (forloop id [Val v] y))
                                            otherwise -> Undefined (show x)) vars
                    Output x y -> weval y vars

var_binding :: Id -> [Binding] -> Call
var_binding x [] = ([], Undefined ("Variable " ++ (show x)))
var_binding x (h:t) = if (fst h) == x && snd (snd h) /= Var (fst h)
                      then snd h
                      else var_binding x t
func_binding :: Id -> [Expr] -> [Binding] -> Call
func_binding x args bindings = func_binding' x args bindings bindings
func_binding' x args [] vars = ([], Undefined ("Function " ++ (show x) ++ " doesn't match any existing pattern."))
func_binding' x args (h:t) vars = 
  if (show id) == (show x) &&
      length args == length params &&
      pattern_match params args vars
   then binding
   else func_binding x args t
  where (id, params, expr) =
         (fst h, fst binding, snd binding)
        binding = snd h
pattern_match [] [] vars = True
pattern_match (a:b) (c:d) vars = 
  case a of
    Name n -> pattern_match b d vars
    Split x y -> case weval c vars of
                   Result (List l) -> pattern_match b d vars
                   Result (Str l) -> pattern_match b d vars
                   otherwise -> False
    Pattern v -> if result == Result v 
                 then pattern_match b d vars
                 else case (result, v) of 
                        (Result (List []), Str "") -> pattern_match b d vars
                        (Result (Str ""), List []) -> pattern_match b d vars
                        otherwise -> False
                 where result = weval c vars
is_function id [] = False
is_function id (h:t) = if fst h == id && length (fst (snd h)) > 0 
                       then True 
                       else is_function id t
pointed id vars = case snd $ var_binding id vars of
                    Var v -> pointed v vars
                    otherwise -> id
-- funcall: list of (ID parameter, expression argument)
funcall :: Expr -> [(Id, Expr)] -> [Binding] -> Expr
funcall f [] vars = f
funcall f (h:t) vars = 
  case param of
    Name n -> Def (Name n) argval (funcall f t vars)
              where argval = case arg of
                               Var v -> if is_function (pointed v vars) vars
                                        then Var v
                                        else eager_eval arg vars
                               Func a b -> Func a b
                               otherwise -> (eager_eval arg vars)
    Split x y -> case eager_eval arg vars of
                    Val (List l) -> if length l > 0 then Def (Name x) (eager_eval (head l) vars) (
                                                         Def (Name y) (Val (List (tail l))) (
                                                         funcall f t vars))
                                                    else Undefined "Can't split empty list"
                    Val (Str l) -> if length l > 0 then Def (Name x) (Val (Str ([head l]))) (
                                                        Def (Name y) (Val (Str (tail l))) (
                                                        funcall f t vars))
                                                   else Undefined "Can't split empty string"
    Pattern _ -> funcall f t vars
    where param = fst h
          arg = snd h
eager_eval :: Expr -> [Binding] -> Expr
eager_eval x vars = case (weval (x) vars) of
                      Result r -> Val r
                      Exception s -> Undefined s
forloop :: Id -> [Expr] -> Expr -> [Expr]
forloop id [] y = []
forloop id (h:t) y = [Def id h y] ++ (forloop id t y)
                                
eval :: Expr -> [Binding] -> Calculation
eval exp bindings = weval exp bindings
