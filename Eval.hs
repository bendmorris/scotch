module Eval where

import Types
import Calc

-- eval: computes the result of an expression as a Calculation
weval :: Expr -> [Binding] -> Calculation
weval exp vars = case exp of
                    Import s -> Incomplete (Placeholder)
                    Undefined s -> Exception ("Undefined: " ++ s)
                    Placeholder -> Incomplete (Placeholder)
                    Skip -> Result Null
                    Val (List l) -> Result (List [(case (weval item vars) of
                                                     Result r -> Val r
                                                     Exception e -> Undefined e
                                                     Incomplete x -> x
                                                     ) 
                                                     | item <- l])
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
                    EagerDef id x y -> weval y ((id, ([], eager_eval x)) : vars)
                    Defun id params x y -> weval y ((id, (params, x)) : vars)
                    Var x -> weval (snd definition) vars
                             where definition = var_binding x vars
                    Func f args -> if length (fst definition) > 0 
                                   then weval (funcall (snd definition)
                                               (zip (fst definition) args)) vars
                                   else Exception "Function call doesn't match any existing patterns."
                                   where definition = func_binding f args vars
                    If cond x y -> case (weval cond vars) of
                                     Result (Bit True) -> weval x vars
                                     Result (Bit False) -> weval y vars
                                     otherwise -> Exception "Non-boolean condition"
                    For id x y -> weval (case (weval x vars) of
                                            Result (List l) -> Val (List (forloop id l y))
                                            Result v -> Val (List (forloop id [Val v] y))
                                            otherwise -> Undefined (show x)) vars
                    Range v -> case weval v vars of
                                 Result (NumInt r) -> weval (Val (List [Val (NumInt i) | i <- [1..r]])) vars
                                 otherwise -> Exception "Bad range"
                    Output x y -> weval y vars
                 where var_binding :: Id -> [Binding] -> Call
                       var_binding x [] = ([], Undefined ("Variable " ++ (show x)))
                       var_binding x (h:t) = if (show $ fst h) == (show x)
                                             then snd h
                                             else var_binding x t
                       func_binding :: Id -> [Expr] -> [Binding] -> Call
                       func_binding x args [] = ([], Undefined ("Function " ++ (show x)))
                       func_binding x args (h:t) = if (show id) == (show x) &&
                                                      length args == length params &&
                                                      pattern_match params args
                                                   then binding
                                                   else func_binding x args t
                                                   where (id, params, expr) =
                                                           (fst h, fst binding, snd binding)
                                                         binding = snd h
                       pattern_match [] [] = True
                       pattern_match (a:b) (c:d) = case a of
                                                     Name n -> pattern_match b d
                                                     Split x y -> case weval c vars of
                                                                    Result (List l) -> pattern_match b d
                                                                    Result (Str l) -> pattern_match b d
                                                                    otherwise -> False
                                                     Pattern v -> if result == Result v 
                                                                  then pattern_match b d
                                                                  else case (result, v) of 
                                                                         (Result (List []), Str "") -> pattern_match b d
                                                                         (Result (Str ""), List []) -> pattern_match b d
                                                                         otherwise -> False
                                                                  where result = weval c vars
                       funcall f [] = f
                       funcall f (h:t) = case fst h of
                                            Name _ -> Def (fst h) (eager_eval (snd h)) (funcall f t)
                                            Split x y -> case eager_eval (snd h) of
                                                            Val (List l) -> if length l > 0 then Def (Name x) (eager_eval (head l)) (
                                                                                                 Def (Name y) (Val (List (tail l))) (
                                                                                                 funcall f t))
                                                                                            else Undefined "Can't split empty list"
                                                            Val (Str l) -> if length l > 0 then Def (Name x) (Val (Str ([head l]))) (
                                                                                                Def (Name y) (Val (Str (tail l))) (
                                                                                                funcall f t))
                                                                                           else Undefined "Can't split empty string"
                                            Pattern _ -> funcall f t
                       eager_eval x = case (weval (x) vars) of
                                        Result r -> Val r
                                        Exception s -> Undefined s
                       forloop :: Id -> [Expr] -> Expr -> [Expr]
                       forloop id [] y = []
                       forloop id (h:t) y = [Def id h y] ++ (forloop id t y)
                                
eval :: Expr -> [Binding] -> Calculation
eval exp bindings = weval exp bindings
