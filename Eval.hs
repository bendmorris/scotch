module Eval where

import Types
import Stdlib
import Calc

-- eval: computes the result of an expression as a Calculation
weval :: Expr -> [Binding] -> Calculation
weval exp vars = case exp of
                    Undefined s -> Exception ("Undefined: " ++ s)
                    Skip -> Result Null
                    Val (List l) -> Result (List [(case (weval item vars) of
                                                     Result r -> Val r) | item <- l])
                    Val x -> Result x
                    Add x y -> calc (weval x vars) (weval y vars) (vadd)
                    Sub x y -> calc (weval x vars) (weval y vars) (vsub)
                    Prod x y -> calc (weval x vars) (weval y vars) (vprod)
                    Div x y -> calc (weval x vars) (weval y vars) (vdiv)
                    Exp x y -> calc (weval x vars) (weval y vars) (vexp)
                    Eq x y -> calc (weval x vars) (weval y vars) (veq)
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
                    Defun id params x y -> weval y ((id, (params, x)) : vars)
                    Var x -> if fst definition == [] then weval (snd definition) vars
                                                     else Exception "Function used as variable"
                              where definition = var_binding x vars
                    Func f args -> if length (fst definition) > 0 
                                   then weval (funcall (snd definition)
                                          (zip (fst definition) args)) vars
                                   else Exception "Variable called as function"
                                   where definition = var_binding f vars
                    If cond x y -> case (weval cond vars) of
                                     Result (Bit True) -> weval x vars
                                     Result (Bit False) -> weval y vars
                                     otherwise -> Exception "Non-boolean condition"
                    For id x y -> weval (case x of
                                            Val (List l) -> Val (List (forloop id l y))
                                            Val v -> Val (List (forloop id [Val v] y))) vars
                    Output x y -> weval y vars
                 where var_binding x [] = ([], Undefined ("Variable " ++ x))
                       var_binding x (h:t) = if fst h == x 
                                             then snd h
                                             else var_binding x t
                       funcall f [] = f
                       funcall f (h:t) = Def (fst h) (snd h) (funcall f t)
                       forloop :: Id -> [Expr] -> Expr -> [Expr]
                       forloop id [] y = []
                       forloop id (h:t) y = [Def id h y] ++ (forloop id t y)
                                
eval :: Expr -> Calculation
eval exp = weval exp Stdlib.stdlib
