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

inparams :: Id -> [(Id, Expr)] -> Expr
inparams x [] = Placeholder
inparams x (h:t) = if x == (fst h) then snd h else inparams x t

inparamsid :: Id -> [(Id, Expr)] -> (Id, [Expr])
inparamsid x [] = (x, [])
inparamsid x (h:t) = if x == (fst h) then
                       case snd h of
                         Var v -> (v, [])
                         Val (HFunc h) -> (h, [])
                         Func f args -> (f, args)
                         otherwise -> inparamsid x t
                      else inparamsid x t

substitute :: Expr -> [(Id, Expr)] -> Expr
substitute exp params =
  case exp of
    Var x -> if newname == Placeholder then Var x else newname
             where newname = inparams x params
    Val (List l) -> Val (List ([substitute e params | e <- l]))
    Val (Proc p) -> Val (Proc ([substitute e params | e <- p]))
    Val v -> Val v
    Subs n x -> Subs (substitute n params) (substitute x params)
    Add x y -> Add (substitute x params) (substitute y params)
    Sub x y -> Sub (substitute x params) (substitute y params)
    Prod x y -> Prod (substitute x params) (substitute y params)
    Div x y -> Div (substitute x params) (substitute y params)
    Exp x y -> Exp (substitute x params) (substitute y params)
    Eq x y -> Eq (substitute x params) (substitute y params)
    InEq x y -> InEq (substitute x params) (substitute y params)
    Gt x y -> Gt (substitute x params) (substitute y params)
    Lt x y -> Lt (substitute x params) (substitute y params)
    And x y -> And (substitute x params) (substitute y params)
    Or x y -> Or (substitute x params) (substitute y params)
    Not x -> Not (substitute x params)
    EagerDef id x y -> EagerDef id (substitute x params) (substitute y params)
    Def id x y -> Def id (substitute x params) (substitute y params)
    Defun id p x y -> Defun id p (substitute x params) (substitute y params)
    If x y z -> If (substitute x params) (substitute y params) (substitute z params)
    Func f args -> case inparamsid f params of
                     (f', []) -> Func f' [substitute arg params | arg <- args]
                     (f', otherargs) -> Func f' [substitute arg params | arg <- otherargs ++ args]
    Output x y -> Output (substitute x params) (substitute y params)
    otherwise -> otherwise

-- eval: computes the result of an expression as a Calculation
weval :: Expr -> [Binding] -> Calculation
weval exp vars = 
  case exp of
    Import s -> Incomplete (Placeholder)
    Undefined s -> Exception (s)
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
    Subs n x -> case (weval x vars) of
                  Result (List l) -> case (weval n vars) of
                                       Result (NumInt n) -> if n >= 0 && 
                                                               n < (fromIntegral (length l))
                                                            then weval (l !! (fromIntegral n)) vars
                                                            else Exception ("Member " ++ show n ++ " not in list")
                                       otherwise -> Exception ("Non-numerical subscript " ++ show otherwise)
                  Result (Str s) -> case (weval n vars) of
                                       Result (NumInt n) -> if n >= 0 && 
                                                               n < (fromIntegral (length s))
                                                            then Result (Str ([s !! (fromIntegral n)]))
                                                            else Exception ("Member " ++ show n ++ " not in list")
                                       otherwise -> Exception ("Non-numerical subscript " ++ show otherwise)
                  otherwise -> Exception "Subscript of non-list"
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
    Defun id params x y -> weval y ((id, (params, x)) : 
                                    (id, ([], Val (HFunc id))) : 
                                    vars)
    Defproc id params x y -> weval y ((id, (params, Val (Proc x))) : 
                                      (id, ([], Val (HFunc id))) : 
                                      vars)
    Var x -> weval (snd definition) vars
             where definition = var_binding x vars
    Func f args -> case vardef of
                     Val (HFunc (h)) -> if length params > 0 
                                        then weval (substitute expr (funcall (zip params args))) vars
                                        else case snd $ var_binding fp vars of
                                               Func f' args' -> weval (Func f' (args' ++ args)) vars
                                               otherwise -> Exception $ "Function " ++ (show f) ++ " doesn't match any existing pattern."
                     Func f' args' -> weval (Func f' (args' ++ args)) vars
                     otherwise -> Exception $ "Variable " ++ (show f) ++ " isn't a function"
                   where fp = case vardef of
                                Val (HFunc (f')) -> f'
                                otherwise -> f
                         vardef = snd $ var_binding f vars
                         definition = func_binding fp args vars
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
       var_binding x [] = ([], Undefined ("Undefined variable " ++ show x))
       var_binding x (h:t) = if (fst h) == x && 
                                length (fst (snd h)) == 0 && 
                                snd (snd h) /= Var (fst h)
                             then case snd (snd h) of
                                    Var v -> if v == x then var_binding x t
                                                       else var_binding v vars
                                    otherwise -> snd h
                             else var_binding x t
       func_binding :: Id -> [Expr] -> [Binding] -> Call
       func_binding x args [] = ([], Undefined ("Function " ++ (show x) ++ " doesn't match any existing pattern."))
       func_binding x args (h:t) = if (show id) == (show x) &&
                                      length args == length params &&
                                      pattern_match params args
                                   then binding
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
       funcall :: [(Id, Expr)] -> [(Id, Expr)]
       funcall [] = []
       funcall (h:t) = 
         case param of
            Name n -> h : funcall t
            Split x y -> case eager_eval arg of
                            Val (List l) -> if length l > 0 then (Name x, eager_eval (head l)) :
                                                                 (Name y, Val (List (tail l))) :
                                                                 funcall t
                                                            else [(Name x, Undefined "Can't split empty list")]
                            Val (Str l) -> if length l > 0 then (Name x, Val (Str [head l])) :
                                                                (Name y, Val (Str (tail l))) :
                                                                funcall t
                                                           else [(Name x, Undefined "Can't split empty string")]
            Pattern _ -> funcall t
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
