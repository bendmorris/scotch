module Eval where

import System.Directory
import Types
import Calc
import Substitute

-- evalList: checks a list for exceptions
evalList [] = Result (Bit True)
evalList (h:t) = case h of                   
                   Undefined e -> Exception e
                   Val (UndefinedValue e) -> Exception e
                   otherwise -> evalList t
                   
left [] 0 = []
left (h:t) 0 = []
left (h:t) n = h : (left t (n - 1))

-- eval: computes the result of an expression as a Calculation
weval :: Expr -> [Binding] -> Calculation
weval exp vars = 
  case exp of
    Import s -> Incomplete (Placeholder)
    Undefined s -> Exception (s)
    Placeholder -> Incomplete (Placeholder)
    Skip -> Result Null
    ListExpr l -> case (evalList l) of
                    Result _ -> case evalList [Val item | item <- l'] of
                                  Exception e -> Exception e
                                  otherwise -> Result $ List l'
                                where l' = [case eval item vars of
                                              Result r -> r
                                              Exception e -> UndefinedValue (e)
                                              otherwise -> UndefinedValue (show otherwise)
                                            | item <- l]
                    Exception e -> Exception e
                  where r = [(case (weval item vars) of
                                Result r -> Val r
                                Exception e -> Undefined e
                                Incomplete i -> i
                              ) | item <- l]
    Val x -> case x of
               UndefinedValue s -> Exception s
               otherwise -> Result x
    ToInt x -> case (weval x vars) of
                 Result (NumInt i) -> Result $ NumInt i
                 Result (NumFloat f) -> Result $ NumInt (truncate f)
                 Result (Str s) -> Result $ NumInt (read s)
                 otherwise -> Exception ("Can't convert " ++ show otherwise ++ " to integer.")
    ToFloat x -> case (weval x vars) of
                         Result (NumInt i) -> Result $ NumFloat (fromIntegral i)
                         Result (NumFloat f) -> Result $ NumFloat f
                         Result (Str s) -> Result $ NumFloat (read s :: Double)
                         otherwise -> Exception ("Can't convert " ++ show otherwise ++ " to float.")
    ToStr x -> case (weval x vars) of                       
                 Result (Str s) -> Result $ Str s
                 otherwise -> Result $ Str (show otherwise)
    Subs n x -> case (weval x vars) of
                  Result (List l) -> case (weval n vars) of
                                       Result (NumInt n) -> if n >= 0 && 
                                                               n < (fromIntegral (length l))
                                                            then Result (l !! (fromIntegral n))
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
    EagerDef id x y -> weval y ((id, ([], eager_eval x vars)) : vars)
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
                                        then case expr of
                                               Func f' args' -> if fp == f' 
                                                                then tailcall fp args args'
                                                                else newcall
                                               otherwise -> newcall
                                        else case snd $ var_binding fp vars of
                                               Func f' args' -> weval (Func f' (args' ++ args)) vars
                                               otherwise -> Exception $ show expr
                     Func f' args' -> weval (Func f' (args' ++ args)) vars
                     otherwise -> Exception $ "Variable " ++ (show f) ++ " isn't a function"
                   where fp = case vardef of
                                Val (HFunc (f')) -> f'
                                otherwise -> f
                         vardef = snd $ var_binding f vars
                         definition = func_binding fp args vars
                         params = fst definition
                         expr = snd definition
                         newcall = weval (substitute expr (funcall (zip params args))) vars
                         tailcall f args args' = if definition' == definition 
                                                  then tailcall f [case weval (substitute (args' !! n) (funcall (zip params args))) vars of
                                                                     Result r -> Val r
                                                                     otherwise -> Undefined $ show otherwise
                                                                   | n <- [0 .. (length args') - 1]] args'
                                                  else weval (substitute (snd definition') (funcall (zip (fst definition') args))) vars
                                                 where definition' = func_binding f args vars
                                                 
    If cond x y -> case (weval cond vars) of
                     Result (Bit True) -> weval x vars
                     Result (Bit False) -> weval y vars
                     Exception e -> Exception e
                     otherwise -> Exception $ "Non-boolean condition " ++ show cond
    For id x y -> weval (case (weval x vars) of
                            Result (List l) -> ListExpr (forloop id [Val item | item <- l] y)
                            Result v -> ListExpr (forloop id [Val v] y)
                            otherwise -> Undefined (show x)) vars
    Range start end step -> case (weval start vars) of
                              Result v -> case v of
                                            NumInt i -> case (weval end vars) of
                                                          Result w -> case w of
                                                                        NumInt j -> case (weval step vars) of
                                                                                      Result u -> case u of
                                                                                                    NumInt k -> Result $ List [NumInt x | x <- [i, i+k .. j]]
                                                                                                    otherwise -> Exception "Non-integer argument in range"
                                                                                      Exception e -> Exception e
                                                                                      otherwise -> Exception "Non-integer argument in range"
                                                                        otherwise -> Exception "Non-integer argument in range"
                                                          Exception e -> Exception e
                                                          otherwise -> Exception "Non-integer argument in range"
                                            otherwise -> Exception "Non-integer argument in range"
                              Exception e -> Exception e
                              otherwise -> Exception "Non-integer argument in range"
    Output x -> case (weval x vars) of
                  Result (Str s) -> PrintOutput s
                  Result r -> PrintOutput (show r)
                  otherwise -> otherwise
    FileObj f -> case weval f vars of
                   Result (Str s) -> Result $ File s
                   otherwise -> Exception "Non-string filename"
    FileRead f -> Incomplete (Placeholder)
    FileWrite f x -> case (weval f vars) of
                       Result (File f) -> case (weval x vars) of
                                            Result (Str s) -> FileOutput f s
                                            otherwise -> Exception $ "Write non-string " ++ show otherwise
                       otherwise -> Exception $ "Write to non-file " ++ show otherwise
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
            Split x y -> case eager_eval arg vars of
                           Val (List l) -> if length l > 0 then (Name x, Val (head l)) :
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
       forloop :: Id -> [Expr] -> Expr -> [Expr]
       forloop id [] y = []
       forloop id (h:t) y = [Def id h y] ++ (forloop id t y)
                                
eval :: Expr -> [Binding] -> Calculation
eval exp bindings = weval exp bindings

eager_eval x vars = case (weval (x) vars) of
                      Result r -> Val r
                      Exception s -> Undefined s

iolist :: [IO Expr] -> IO [Expr]
iolist [] = do return []
iolist (h:t) = do item <- h
                  rest <- iolist t
                  return (item:rest)

subfile :: Expr -> [Binding] -> IO Expr
subfile exp vars =
  case exp of
    Val (Proc p) -> do list <- iolist [subfile e vars | e <- p]
                       return $ Val (Proc list)
    ToInt x -> do x' <- subfile x vars
                  return $ ToInt x'
    ToFloat x -> do x' <- subfile x vars
                    return $ ToFloat x'
    ToStr x -> do x' <- subfile x vars
                  return $ ToStr x'
    ListExpr l -> do list <- iolist [subfile e vars | e <- l]
                     return $ ListExpr list
    Subs x y -> do x' <- subfile x vars
                   y' <- subfile y vars
                   return $ Subs x' y'
    Add x y -> do x' <- subfile x vars
                  y' <- subfile y vars
                  return $ Add x' y'
    Sub x y -> do x' <- subfile x vars
                  y' <- subfile y vars
                  return $ Sub x' y'
    Prod x y -> do x' <- subfile x vars
                   y' <- subfile y vars
                   return $ Prod x' y'
    Div x y -> do x' <- subfile x vars
                  y' <- subfile y vars
                  return $ Div x' y'
    Exp x y -> do x' <- subfile x vars
                  y' <- subfile y vars
                  return $ Exp x' y'
    Eq x y -> do x' <- subfile x vars
                 y' <- subfile y vars
                 return $ Eq x' y'
    InEq x y -> do x' <- subfile x vars
                   y' <- subfile y vars
                   return $ InEq x' y'
    Gt x y -> do x' <- subfile x vars
                 y' <- subfile y vars
                 return $ Gt x' y'
    Lt x y -> do x' <- subfile x vars
                 y' <- subfile y vars
                 return $ Lt x' y'
    And x y -> do x' <- subfile x vars
                  y' <- subfile y vars
                  return $ And x' y'
    Or x y -> do x' <- subfile x vars
                 y' <- subfile y vars
                 return $ Or x' y'
    Not x -> do x' <- subfile x vars
                return $ Not x'
    EagerDef id x y -> do x' <- subfile x vars'
                          y' <- subfile y vars'
                          return $ EagerDef id x' y'
                          where vars' = ((id, ([], eager_eval x vars)) : vars)
    Def id x y -> do x' <- subfile x vars'
                     y' <- subfile y vars'
                     return $ Def id x' y'
                     where vars' = ((id, ([], x)) : vars)
    Defun id p x y -> do x' <- subfile x vars'
                         y' <- subfile y vars'
                         return $ Defun id p x' y'
                         where vars' = ((id, (p, x)) : 
                                        (id, ([], Val (HFunc id))) : 
                                        vars)
    If x y z -> do x' <- subfile x vars
                   y' <- subfile y vars
                   z' <- subfile z vars
                   return $ If x' y' z'
    For id x y -> do x' <- subfile x vars
                     y' <- subfile y vars
                     return $ For id x' y'
    Output x -> do x' <- subfile x vars
                   return $ Output x'
    FileRead f -> do case weval f vars of
                       Result (File f) -> do exists <- doesFileExist f
                                             case exists of 
                                               True -> do contents <- readFile f
                                                          return $ Val $ Str contents
                                               False -> return $ Undefined "File does not exist"
                       otherwise -> do return $ Undefined "Invalid file"
    otherwise -> do return otherwise
