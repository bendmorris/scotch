module Eval where

import Types
import Calc

-- evalList: checks a list for exceptions
evalList [] = Result (Bit True)
evalList (h:t) = case h of
                   Val r -> evalList t
                   Undefined e -> Exception e
                   otherwise -> Exception "Non-value used in list."
                   
left [] 0 = []
left (h:t) 0 = []
left (h:t) n = h : (left t (n - 1))

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
                    EagerDef id x y -> weval y ((id, ([], eager_eval x)) : vars)
                    Defun id params x y -> weval y ((id, (params, x)) : (id, ([], Val (HFunc id))): vars)
                    Var x -> weval (snd definition) vars
                             where definition = var_binding x vars
                    Func f args -> case vardef of
                                     Val (HFunc (h)) -> if length params > 0 
                                                        then weval (funcall (expr)
                                                                    (zip params args)) vars'
                                                        else case snd $ var_binding fp vars of
                                                               Func f' args' -> weval (Func f' (args' ++ args)) vars'
                                                               otherwise -> Exception $ "Function " ++ (show f) ++ " doesn't match any existing pattern."
                                     otherwise -> Exception $ "Variable " ++ (show f) ++ " isn't a function"
                                   where fp = case vardef of
                                                Val (HFunc (f')) -> f'
                                         vardef = snd $ var_binding f vars
                                         binding = func_binding fp args vars
                                         definition = fst binding
                                         vars' = snd binding
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
                 where var_binding :: Id -> [Binding] -> Call
                       var_binding x [] = ([], Undefined ("Variable " ++ show x))
                       var_binding x (h:t) = if (fst h) == x && 
                                                length (fst (snd h)) == 0 && 
                                                snd (snd h) /= Var (fst h)
                                             then case snd (snd h) of
                                                    Var v -> if v == x then var_binding x t
                                                                       else var_binding v vars
                                                    otherwise -> snd h
                                             else var_binding x t
                       func_binding :: Id -> [Expr] -> [Binding] -> (Call, [Binding])
                       func_binding x args [] = (([], Undefined ("Function " ++ (show x) ++ " doesn't match any existing pattern.")), [])
                       func_binding x args (h:t) = if (show id) == (show x) &&
                                                      length args == length params &&
                                                      pattern_match params args
                                                   then (binding, vars)
                                                   else func_binding x args t
                                                   where (id, params, expr) =
                                                           (fst h, fst binding, snd binding)
                                                         binding = snd h
                       pattern_match [] [] = True
                       pattern_match (a:b) (c:d) = 
                         case a of
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
                       is_function id [] = False
                       is_function id (h:t) = if fst h == id && length (fst (snd h)) > 0 
                                              then True 
                                              else is_function id t
                       pointed id = case snd $ var_binding id vars of
                                      Var v -> pointed v
                                      otherwise -> id
                       -- funcall: list of (ID parameter, expression argument)
                       funcall f [] = f
                       funcall f (h:t) = 
                         case param of
                            Name n -> case argval of
                                        Var (Name v) -> if n == v then funcall f t
                                                                  else EagerDef (Name n) argval (funcall f t)
                                        otherwise -> EagerDef (Name n) argval (funcall f t)
                                      where argval = case arg of
                                                       Var v -> case snd $ var_binding v vars of
                                                                  Val (HFunc f) -> Val (HFunc f)
                                                                  otherwise -> eager_eval arg
                                                       otherwise -> (eager_eval arg)
                            Split x y -> case eager_eval arg of
                                            Val (List l) -> if length l > 0 then Def (Name x) (eager_eval (head l)) (
                                                                                 Def (Name y) (Val (List (tail l))) (
                                                                                 funcall f t))
                                                                            else Undefined "Can't split empty list"
                                            Val (Str l) -> if length l > 0 then Def (Name x) (Val (Str ([head l]))) (
                                                                                Def (Name y) (Val (Str (tail l))) (
                                                                                funcall f t))
                                                                           else Undefined "Can't split empty string"
                            Pattern _ -> funcall f t
                            where param = fst h
                                  arg = snd h
                       eager_eval x = case (weval (x) vars) of
                                        Result r -> Val r
                                        Exception s -> Undefined s
                       forloop :: Id -> [Expr] -> Expr -> [Expr]
                       forloop id [] y = []
                       forloop id (h:t) y = [Def id h y] ++ (forloop id t y)
                                
eval :: Expr -> [Binding] -> Calculation
eval exp bindings = weval exp bindings
