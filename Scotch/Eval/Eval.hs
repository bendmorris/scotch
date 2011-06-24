{-  This file is part of Scotch.

    Scotch is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Scotch is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOnoR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Scotch.  If not, see <http://www.gnu.org/licenses/>.
-}

module Scotch.Eval.Eval (ieval, subfile) where

import Data.List
import Numeric
import System.Directory
import Scotch.Types.Types
import Scotch.Types.Exceptions
import Scotch.Types.Bindings
import Scotch.Types.Hash
import Scotch.Types.Interpreter
import Scotch.Eval.Calc
import Scotch.Eval.Substitute
import Scotch.Parse.Parse as Parse


{- 
eval: evaluates an expression.
      This function evaluates expressions step by step and should not be assumed to result 
      in full evaluation; rather, eval should be run until the result is the same as the initial input.
-}
eval :: Expr -> VarDict -> InterpreterSettings -> Bool -> Expr
eval exp [] settings rw = eval exp emptyHash settings rw
eval oexp vars settings rw = 
  if exp /= oexp 
  then exp
  else case exp of
  Var id ->             if length (qualVarHash id vars) > 0
                        then Val $ Hash $ makeHash strHash (qualVarHash id vars) emptyHash
                        else if length (qualVarHash ("local." ++ id) vars) > 0
                             then Val $ Hash $ makeHash strHash (qualVarHash ("local." ++ id) vars) emptyHash
                             else Var id
  Call x [] -> x
  Call (Call id args) args' -> eval' $ Call id (args ++ args')
  Call (Var id) args -> if fullEval (Var id) eval' == Var id
                        then Call (Var id) [fullEval arg eval' | arg <- args]
                        else Call (fullEval (Var id) eval') args
  Call (Val (Lambda ids expr)) args ->
                        if length ids == length args
                        then substitute expr (zip [Var id | id <- ids] args)
                        else exp
  Call (Val (NumInt i)) args -> Prod (Val (NumInt i)) (totalProd args)
  Call (Val (NumFloat i)) args -> Prod (Val (NumFloat i)) (totalProd args)
  Call x args ->        Call (fullEval x eval') args
  EvalExpr x ->         case eval' x of
                          Val (Str s) -> case length evaled of
                                           0 -> Skip
                                           1 -> evaled !! 0
                                           otherwise -> Val $ Proc $ evaled
                                         where evaled = [snd i | i <- Parse.read "" s]
                          otherwise -> EvalExpr otherwise
  Import s t ->         Import s t
  Take n x ->           case n of
                          Val (NumInt i) -> case x of
                                              List l -> List (take i' l)
                                              Range from to step -> case eval' (Range from to step) of
                                                                      List l -> List (take i' l)
                                                                      Exception e -> Exception e
                                                                      otherwise -> Take n otherwise
                                              Val (Str s) -> Val $ Str $ take i' s
                                              For id x y conds -> TakeFor id x y conds i
                                              Exception e -> Exception e
                                              Add (List l) (y) -> if length t == i'
                                                                  then List t
                                                                  else Take n (eval' x)
                                                                  where t = take i' l
                                              otherwise -> Take n (eval' x)
                                            where i' = fromIntegral i
                          Exception e -> Exception e
                          otherwise -> Take (eval' otherwise) x
  List l ->             case (validList l) of
                          Val _ -> List [eval' i | i <- l]
                          Exception e -> Exception e
  HashExpr l ->         Val $ Hash $ makeHash strHash
                                     [(case eval' (fst i) of
                                         Val (Str s) -> s
                                         otherwise -> show otherwise,
                                       snd i)
                                      | i <- l] emptyHash
  Val x ->              case x of
                          Undefined s -> Exception s
                          otherwise -> Val x
  ToInt x ->            case eval' x of
                          Val (NumInt i) -> Val $ NumInt i
                          Val (NumFloat f) -> Val $ NumInt (truncate f)
                          Val (Str s) -> Val $ NumInt (Prelude.read s)
                          Exception e -> Exception e
                          otherwise -> ToInt otherwise
  ToFloat x ->          case eval' x of
                          Val (NumInt i) -> Val $ NumFloat $ fromIntegral i
                          Val (NumFloat f) -> Val $ NumFloat f
                          Val (Str s) -> Val $ NumFloat (Prelude.read s :: Double)
                          Exception e -> Exception e
                          otherwise -> ToFloat otherwise
  ToStr x ->            case eval' x of
                          Val (Str s) -> Val $ Str s
                          Val (NumFloat f) -> Val $ Str $ showFFloat Nothing f ""
                          Val (Undefined u) -> Exception u
                          Val v -> Val $ Str (show v)
                          Exception e -> Exception e
                          ToStr y -> ToStr y
                          otherwise -> if otherwise == x
                                       then Val $ Str $ show x
                                       else ToStr $ eval' otherwise
  ToList x ->           case eval' x of
                          List l -> List l
                          Val (Str s) -> List [Val (Str [c]) | c <- s]
                          Val (Hash h) -> List [List [Val (Str (fst l)), snd l] | e <- h, l <- e]
                          Val (File f) -> Call (Var "std.lib.split") [FileRead (Val (File f)), Val (Str "\n")]
                          FileObj f -> Call (Var "std.lib.split") [FileRead (f), Val (Str "\n")]
                          Exception e -> Exception e
                          Val v -> List [Val v]
                          otherwise -> ToList $ eval' otherwise
  Subs n x ->           case x of
                          List l ->       case n' of
                                            Val (NumInt n) -> if n >= 0
                                                              then l !! (fromIntegral n)
                                                              else l !! ((length l) + (fromIntegral n))
                                            List l' ->        List [Subs i (List l) | i <- l']
                                            otherwise ->      exNonNumSubs otherwise
                          Val (Str s) ->  case n' of
                                            Val (NumInt n) -> if n >= 0
                                                              then Val (Str ([s !! (fromIntegral n)]))
                                                              else Val (Str ([s !! ((length s) + (fromIntegral n))]))
                                            List l' ->        List [Subs i (Val (Str s)) | i <- l']
                                            otherwise ->      exNonNumSubs otherwise
                          Val (Hash l) -> case n' of
                                            Exception e -> Exception e
                                            List l' ->        List [Subs i (Val (Hash l)) | i <- l']
                                            otherwise -> case fullEval (ToStr otherwise) eval' of
                                                           Val (Str s) ->    case hashMember strHash s l of
                                                                               Just x -> x
                                                                               Nothing -> exNotInHash s
                                                           Exception e ->    Exception e
                                                           ToStr s ->        Subs s x
                                                           otherwise ->      Subs otherwise (Val (Hash l))
                          Call (Var f) args ->  case n' of
                                                  Val (NumInt n) -> if n >= 0
                                                                    then eval' $ Subs (Val (NumInt n)) (eval' (Take (Val (NumInt ((fromIntegral n) + 1))) (Call (Var f) args)))
                                                                    else Subs (Val (NumInt n)) (eval' x)
                                                  List l' ->        List [Subs i f' | i <- l']
                                                                    where f' = (Call (Var f) args)
                                                  otherwise ->      Subs otherwise (eval' x)
                          otherwise ->    Subs n (eval' otherwise)
                        where n' = fullEval n eval'
  Concat x y ->         eval' (Add x y)
  Add x y ->            case x of
                          Exception e ->    Exception e
                          List l ->         case y of
                                              Exception e -> Exception e
                                              List l' -> List $ l ++ l'
                                              Val v -> vadd (strict settings) x y
                                              Add a (Call id args) -> Add (eval' (Add x a)) (Call id args)
                                              otherwise -> nextOp
                          Val v ->          case y of
                                              Exception e -> Exception e
                                              List l -> vadd (strict settings) x y
                                              Val v -> vadd (strict settings) x y
                                              otherwise -> nextOp
                          Add a b ->        {-if (eval' x) == x
                                            then Add a (Add b y)
                                            else -}nextOp
                          otherwise ->      nextOp
                        where nextOp = if vadd (strict settings) x y == Add x y
                                       then Add (eval' x) (eval' y)
                                       else operation x y vadd Add
  Sub x y ->            operation x y vsub Sub
  Prod x y ->           {-if nextOp == Prod x y
                        then if eval' (Prod y x) == Prod y x
                             then nextOp
                             else case x of
                                    Prod a b -> if eval' x == x
                                                then Prod a (Prod b y)
                                                else nextOp
                                    otherwise -> case y of
                                                   Prod a b -> if eval' y == y
                                                               then Prod (Prod x a) b
                                                               else nextOp
                                                   otherwise -> nextOp
                        else -}nextOp
                        where nextOp = operation x y vprod Prod
  Div x y ->            operation x y vdiv Div
  Mod x y ->            operation x y vmod Mod
  Exp x y ->            operation x y vexp Exp
  Eq x y ->             operation (fullEval x eval') (fullEval y eval') veq Eq
  InEq x y ->           eval' (Prod (operation x y veq Eq) (Val (NumInt (-1))))
  Gt x y ->             operation x y vgt Gt
  Lt x y ->             operation x y vlt Lt
  And x y ->            case eval' x of
                          Val (Bit True) -> case eval' y of
                                              Val (Bit True) -> Val (Bit True)
                                              Val (Bit False) -> Val (Bit False)
                                              otherwise -> And (eval' x) (eval' y)
                          Val (Bit False) -> Val (Bit False)
                          otherwise -> And (eval' x) (eval' y)
                        where err = exTypeMismatch (eval' x) (eval' y) "and"
  Or x y ->             case eval' x of
                          Val (Bit True) -> Val (Bit True)                                            
                          Val (Bit False) -> case eval' y of
                                               Val (Bit b) -> Val (Bit b)
                                               otherwise -> Or (eval' x) (eval' y)
                          otherwise -> Or (eval' x) (eval' y)
                        where err = exTypeMismatch (eval' x) (eval' y) "or"
  Not x ->              case eval' x of
                          Exception s -> Exception s
                          Val (Bit b) -> Val $ Bit $ not b
                          otherwise -> Not otherwise
  Def f x y ->          Def f x y
  EagerDef f x y ->     if eval' x == x
                        then EagerDef f x y
                        else EagerDef f (eval' x) y
  If cond x y ->        case eval' cond of
                          Val (Bit True) -> x
                          Val (Bit False) -> y
                          Exception e -> Exception e
                          otherwise -> If otherwise x y
  Case check cases ->   caseExpr check (reverse cases)
  For id x y conds ->   case eval' x of
                          List l ->         List [substitute y [(Var id, item)] | item <- l,
                                                      allTrue [substitute cond [(Var id, item)] | cond <- conds]
                                                      ]
                          Exception e ->    Exception e
                          otherwise ->      For id otherwise y conds
  TakeFor id x y conds n -> case eval' x of
                          List l ->         List     (take (fromIntegral n)
                                                     [substitute y [(Var id, item)] | item <- l,
                                                      allTrue [substitute cond [(Var id, item)] | cond <- conds]
                                                      ])
                          Exception e ->    Exception e
                          otherwise ->      TakeFor id otherwise y conds n
  Range from to step -> case from of
                          Val (NumInt i) -> case to of
                                              Val (NumInt j) -> case step of
                                                                  Val (NumInt k) -> List [Val (NumInt x) | x <- [i, i+k .. j]]
                                                                  Exception e -> Exception e
                                                                  otherwise -> Range from to (eval' step)
                                              Skip -> case (eval' step) of
                                                        Val (NumInt k) -> List [Val (NumInt x) | x <- [i, i+k ..]]
                                                        Exception e -> Exception e
                                                        otherwise -> Range from Skip otherwise
                                              Exception e -> Exception e
                                              otherwise -> Range from (eval' to) step
                          Exception e -> Exception e
                          otherwise -> Range (eval' from) to step
  FileObj f ->          case eval' f of
                          Val (Str s) -> Val $ File s
                          otherwise -> FileObj otherwise
  Output x ->           Output (eval' x)
  FileRead f ->         FileRead (eval' f)
  FileWrite f x ->      case eval' f of
                          Val (File f) -> case eval' x of
                                            Val (Str s) -> FileWrite (Val (File f)) (Val (Str s))
                                            otherwise -> FileWrite (Val (File f)) otherwise
                          otherwise -> FileWrite otherwise x
  FileAppend f x ->     case eval' f of
                          Val (File f) -> case eval' x of
                                            Val (Str s) -> FileAppend (Val (File f)) (Val (Str s))
                                            otherwise -> FileAppend (Val (File f)) otherwise
                          otherwise -> FileAppend otherwise x
  UseRule r x ->        case r of
                          Rule r -> eval' (rule x r)
                          List l -> if all ((/=) [Skip]) l'
                                    then UseRule (Rule (allRules l' [])) x
                                    else exInvalidRule r
                                    where l' = [case fullEval i eval' of
                                                  Rule r' -> r'
                                                  otherwise -> [Skip]
                                                | i <- l]
                          otherwise -> UseRule (eval' r) x
                        where rule x (h:t) = case h of
                                               Def a b c -> Def a b (rule x t)
                                               EagerDef a b c -> EagerDef a b (rule x t)
                                               otherwise -> rule x t
                              rule x [] = x
                              allRules (h:t) a = allRules t (a ++ h)
                              allRules [] a = a
  otherwise ->          otherwise
 where operation x y f g = if calc x y f (strict settings) == g x y
                           then g (eval' x) (eval' y)
                           else calc x y f (strict settings)
                             
       allTrue [] = True
       allTrue (h:t) = case eval' h of
                         Val (Bit True) -> allTrue t
                         Exception e -> False
                         Val v -> False
                         otherwise -> if otherwise == h then False else allTrue (otherwise : t)
       caseExpr check [] = Call (Var "case") [fullEval check eval']
       caseExpr check (h:t) = Def (Call (Var "case") [fst h]) (snd h) (caseExpr check t)
       evalArgs x = case x of
                      Call a b -> Call (evalArgs a) ([fullEval i eval' | i <- b])
                      otherwise -> otherwise
       exp = if rw
             then rewrite (evalArgs oexp) (vars !! exprHash oexp) (vars !! exprHash oexp) eval'
             else oexp
       eval' expr = eval expr vars settings rw
       

totalProd [] = Val (NumInt 1)       
totalProd (h:t) = if t == []
                  then h
                  else Prod h (totalProd t)
                                    

iolist :: [IO Expr] -> IO [Expr]
iolist [] = do return []
iolist (h:t) = do item <- h
                  rest <- iolist t
                  return (item:rest)

-- ieval: evaluates an expression completely, replacing I/O operations as necessary
ieval :: InterpreterSettings -> Expr -> VarDict -> [Expr] -> IO Expr
ieval settings expr vars last =
  do subbed <- subfile expr vars
     let result = eval subbed vars settings True
     if isInfixOf [result] last
      then return (last !! 0)
      else do if (verbose settings) && length last > 0 
                then putStrLn (show (last !! 0))
                else return ()
              result' <- case expr of
                           Def id x y -> do return $ (y, makeVarDict [(id, x)] vars)
                           EagerDef id x y -> do x' <- ieval settings x vars (expr : last')
                                                 return $ (y, makeVarDict [(id, x')] vars)
                           otherwise -> do return (result, vars)
              ieval settings (fst result') (snd result') (expr : last')
              where last' = take 4 last

-- subfile: substitutes values for delayed I/O operations
subfile :: Expr -> VarDict -> IO Expr
subfile exp vars =
  case exp of
    Take n x -> do n' <- subfile n vars
                   x' <- subfile x vars
                   return $ Take n' x'
    TakeFor a b c d e -> do b' <- subfile b vars
                            c' <- subfile c vars
                            d' <- iolist [subfile i vars | i <- d]
                            return $ TakeFor a b' c' d' e
    ToInt x -> do x' <- subfile x vars
                  return $ ToInt x'
    ToFloat x -> do x' <- subfile x vars
                    return $ ToFloat x'
    ToStr x -> do x' <- subfile x vars
                  return $ ToStr x'
    ToList l -> do l' <- subfile l vars
                   return $ ToList l'
    List l ->     do list <- iolist [subfile e vars | e <- l]
                     return $ List list
    HashExpr l -> do list1 <- iolist [subfile (fst e) vars | e <- l]
                     list2 <- iolist [subfile (snd e) vars | e <- l]
                     return $ HashExpr (zip list1 list2)
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
    Mod x y -> do x' <- subfile x vars
                  y' <- subfile y vars
                  return $ Mod x' y'
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
    EagerDef id x y -> do x' <- subfile x vars
                          y' <- subfile y vars
                          return $ EagerDef id x' y'
    Def id x y -> do y' <- subfile y vars
                     return $ Def id x y'
    If x y z -> do x' <- subfile x vars
                   y' <- subfile y vars
                   z' <- subfile z vars
                   return $ If x' y' z'
    For id x y z -> do x' <- subfile x vars
                       y' <- subfile y vars
                       z' <- iolist [subfile i vars | i <- z]
                       return $ For id x' y' z'
    Output x -> do x' <- subfile x vars
                   return $ Output x'
    Input -> do line <- getLine
                return $ Val (Str line)
    FileObj f -> do f' <- subfile f vars
                    return $ FileObj f'
    FileRead f -> do sub <- subfile f vars
                     case f of
                       Val (File f) -> do exists <- doesFileExist f
                                          case exists of 
                                            True -> do contents <- readFile f
                                                       return $ Val $ Str contents
                                            False -> return $ exFileDNE
                       otherwise -> return $ FileRead f
    otherwise -> do return otherwise
