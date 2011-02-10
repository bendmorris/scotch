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

module Eval where

import Data.List
import Parse
import Numeric
import System.Directory
import Types
import Exceptions
import Bindings
import Calc
import Substitute
import Hash


-- eval: computes the value of an expression as far as possible
eval :: Expr -> VarDict -> Expr
eval exp [] = eval exp emptyHash
eval exp vars = case exp of
  EvalExpr x ->         case eval x vars of
                          Val (Str s) -> case length evaled of
                                           0 -> Skip
                                           1 -> evaled !! 0
                                           2 -> Val $ Proc $ evaled
                                         where evaled = [snd i | i <- Parse.read "" s]
                          otherwise -> EvalExpr otherwise
  Import s t ->         Import s t
  Take n x ->           case n of
                          Val (NumInt i) -> case x of
                                              Val (List l) -> Val (List (take i' l))
                                              ListExpr l -> eval (ListExpr (take i' l)) vars
                                              Range from to step -> case eval (Range from to step) vars of
                                                                      Val (List l) -> Val (List (take i' l))
                                                                      ListExpr l -> eval (ListExpr (take i' l)) vars
                                                                      Exception e -> Exception e
                                                                      otherwise -> otherwise
                                              Val (Str s) -> Val $ Str $ take i' s
                                              For id x y conds -> TakeFor id x y conds i
                                              Exception e -> Exception e
                                              Add (ListExpr l) (y) -> if length t == i'
                                                                      then ListExpr t
                                                                      else Take n (eval x vars)
                                                                      where t = take i' l
                                              Add (Val (List l)) (y) -> if length t == i'
                                                                        then Val $ List t
                                                                        else Take n (eval x vars)
                                                                        where t = take i' l

                                              otherwise -> Take n (eval x vars)
                                            where i' = fromIntegral i
                          Exception e -> Exception e
                          otherwise -> Take (eval otherwise vars) x
  ListExpr l ->         case (validList l) of
                          Val _ -> case validList l'' of
                                     Exception e -> Exception e
                                     otherwise -> if computableList l''
                                                  then Val $ List l'
                                                  else ListExpr [eval i vars | i <- l]
                                   where l' = [case eval item vars of
                                                 Val r -> r
                                                 Exception e -> Undefined e
                                                 otherwise -> InvalidValue
                                               | item <- l]
                                         l'' = [Val item | item <- l']
                          Exception e -> Exception e
  HashExpr l ->         case (validList [snd e | e <- l]) of
                          Val _ -> case validList [Val item | item <- l'] of
                                     Exception e -> Exception e
                                     otherwise -> case validList [Val item | item <- m'] of
                                                    Val _ -> Val $ Hash (makeHash (zip [case eval (fst i) vars of
                                                                                          Val (Str s) -> s
                                                                                          otherwise -> show otherwise
                                                                                        | i <- l] l'))
                                                    Exception e -> Exception e
                                   where l' = [case eval (snd item) vars of
                                                 Val r -> r
                                                 Exception e -> Undefined e
                                                 otherwise -> Undefined (show otherwise)
                                               | item <- l]
                                         m' = [case eval (fst item) vars of
                                                 Val r -> r
                                                 Exception e -> Undefined e
                                                 otherwise -> Undefined (show otherwise)
                                               | item <- l]
                          Exception e -> Exception e
  Val x ->              case x of
                          Undefined s -> Exception s
                          otherwise -> Val x
  ToInt x ->            case (eval x vars) of
                          Val (NumInt i) -> Val $ NumInt i
                          Val (NumFloat f) -> Val $ NumInt (truncate f)
                          Val (Str s) -> Val $ NumInt (Prelude.read s)
                          Exception e -> Exception e
                          otherwise -> ToInt otherwise
  ToFloat x ->          case (eval x vars) of
                          Val (NumInt i) -> Val $ NumFloat $ fromIntegral i
                          Val (NumFloat f) -> Val $ NumFloat f
                          Val (Str s) -> Val $ NumFloat (Prelude.read s :: Double)
                          Exception e -> Exception e
                          otherwise -> ToFloat otherwise
  ToStr x ->            case (eval x vars) of
                          Val (Str s) -> Val $ Str s
                          Val (NumFloat f) -> Val $ Str $ showFFloat Nothing f ""
                          Val (Undefined u) -> Exception u
                          Val v -> Val $ Str (show v)
                          Exception e -> Exception e
                          otherwise -> ToStr (eval otherwise vars)
  ToList x ->           case (eval x vars) of
                          Val (List l) -> Val $ List l
                          ListExpr l -> ListExpr l
                          Val (Str s) -> Val $ List [Str [c] | c <- s]
                          Val (Hash h) -> ListExpr [ListExpr [Val (Str (fst l)), Val (snd l)] | e <- h, l <- e]
                          Val (File f) -> Func (Name "std.lib.split") [FileRead (Val (File f)), Val (Str "\n")]
                          FileObj f -> Func (Name "std.lib.split") [FileRead (f), Val (Str "\n")]
                          Exception e -> Exception e
                          Val v -> Val (List [v])
                          otherwise -> ToList (eval otherwise vars)
  Subs n x ->           case x of
                          Val (List l) -> case (eval n vars) of
                                            Val (NumInt n) -> if n >= 0
                                                              then Val (l !! (fromIntegral n))
                                                              else Val (l !! ((length l) + (fromIntegral n)))
                                            Val (List l') ->  ListExpr [Subs (Val i) (Val (List l)) | i <- l']
                                            otherwise ->      exNonNumSubs otherwise
                          ListExpr l ->   case (eval n vars) of
                                            Val (NumInt n) -> if n >= 0
                                                              then l !! (fromIntegral n)
                                                              else l !! ((length l) + (fromIntegral n))
                                            Val (List l') ->  ListExpr [Subs (Val i) (ListExpr l) | i <- l']
                                            otherwise ->      exNonNumSubs otherwise
                          Val (Str s) ->  case (eval n vars) of
                                            Val (NumInt n) -> if n >= 0
                                                              then Val (Str ([s !! (fromIntegral n)]))
                                                              else Val (Str ([s !! ((length s) + (fromIntegral n))]))
                                            Val (List l') ->  ListExpr [Subs (Val i) (Val (Str s)) | i <- l']
                                            otherwise ->      exNonNumSubs otherwise
                          Val (Hash l) -> case eval n vars of
                                            Exception e -> Exception e
                                            otherwise -> case (eval (ToStr otherwise) vars) of
                                                           Val (Str s) ->    eval (Val $ hashMember s l) vars
                                                           Exception e ->    Exception e
                                                           otherwise ->      Subs otherwise (Val (Hash l))
                          otherwise ->    Subs n (eval otherwise vars)
  Add x y ->            case x of
                          Exception e ->    Exception e
                          ListExpr l ->     case y of
                                              Exception e -> Exception e
                                              ListExpr l' -> ListExpr (l ++ l')
                                              Val v -> Add (eval x vars) y
                                              Add a b -> Add (Add x a) b
                                              otherwise -> Add x (eval y vars)
                          Val v ->          case y of
                                              Exception e -> Exception e
                                              Val v -> vadd x y
                                              otherwise -> Add x (eval y vars)
                          otherwise -> Add (eval x vars) y
  Sub x y ->            operation x y vsub Sub
  Prod x y ->           operation x y vprod Prod
  Div x y ->            operation x y vdiv Div
  Mod x y ->            operation x y vmod Mod
  Exp x y ->            operation x y vexp Exp
  Eq x y ->             operation x y veq Eq
  InEq x y ->           eval (Prod (operation x y veq Eq) (Val (NumInt (-1)))) vars
  Gt x y ->             operation x y vgt Gt
  Lt x y ->             operation x y vlt Lt
  And x y ->            case eval x vars of
                          Val (Bit True) -> case eval y vars of
                                              Val (Bit True) -> Val (Bit True)
                                              Val (Bit False) -> Val (Bit False)
                                              otherwise -> err
                          Val (Bit False) -> Val (Bit False)
                          otherwise -> err
                        where err = exTypeMismatch (eval x vars) (eval y vars) "and"
  Or x y ->             case eval x vars of
                          Val (Bit True) -> Val (Bit True)                                            
                          Val (Bit False) -> case eval y vars of
                                               Val (Bit b) -> Val (Bit b)
                                               otherwise -> err
                          otherwise -> err
                        where err = exTypeMismatch (eval x vars) (eval y vars) "or"
  Not x ->              case eval x vars of
                          Exception s -> Exception s
                          Val r -> case r of
                                     Bit b -> Val (Bit (not b))
                                     otherwise -> exNotBool otherwise
                          otherwise -> Not otherwise
  Def f x y ->          eval (substitute y [(f, x)]) vars
  EagerDef f x y ->     case eval x vars of
                          Exception e -> Exception e
                          Val v -> eval (substitute y [(f, Val v)]) vars
                          otherwise -> EagerDef f (eval otherwise vars) y
  Defun f p x y ->      eval y (addBinding (f, (p, x)) 
                                (addBinding(f, ([], Val (HFunc f)))  
                                 vars))
  Defproc f p x y ->    eval y (addBinding (f, (p, Val (Proc x)))
                                (addBinding (f, ([], Val (HFunc f)))
                                 vars))
  Var x ->              case snd ((varBinding x (vars !! varHash x) vars) !! 0) of
                          Exception e -> if length qualDict > 0 then HashExpr qualDict else Exception e
                                         where allDefs = [binding | i <- vars, binding <- i]
                                               qualDict = [(Val (Str ([(qualName (stripName (fst def))) !! n | n <- [length (qualName (stripName x)) + 1 .. length (qualName (stripName (fst def))) - 1]])), 
                                                            snd (snd def))
                                                           | def <- allDefs,
                                                             length (fst (snd def)) == 0,
                                                             isPrefixOf ((stripName x) ++ ".") (qualName (stripName (fst def)))]
                                               qualName n = if isPrefixOf "local" (stripName x) then n else stripLocal n
                          otherwise -> otherwise
  Func f args ->        case validList evalArgs of
                          Exception e -> Exception e
                          otherwise -> if computableList evalArgs
                                       then functionCall f evalArgs (varBinding f (vars !! varHash f) vars) vars
                                       else Func f evalArgs
                        where evalArgs = [eval arg vars | arg <- args]
  LambdaCall x args ->  case validList evalArgs of
                          Exception e -> Exception e
                          otherwise -> if computableList evalArgs
                                       then case eval x vars of
                                              Exception e -> Exception e
                                              Val (HFunc f) -> eval (Func f evalArgs) vars
                                              Func f args' -> eval (Func f (args' ++ evalArgs)) vars
                                              Val (Lambda params f) -> if length params == length evalArgs
                                                                       then substitute f (zip params evalArgs)
                                                                       else LambdaCall (Val (Lambda params f)) evalArgs
                                              Val v -> exImproperCall v
                                              LambdaCall x' args' -> eval (LambdaCall x' (args' ++ evalArgs)) vars
                                              otherwise -> eval (LambdaCall (eval otherwise vars) evalArgs) vars
                                       else LambdaCall x evalArgs
                        where evalArgs = [eval arg vars | arg <- args]
  If cond x y ->        case eval cond vars of
                          Val (Bit True) -> x
                          Val (Bit False) -> y
                          Exception e -> Exception e
                          otherwise -> If otherwise x y
  Case check (h:t) ->   case (eval check vars) of
                          Exception e -> Exception e
                          Val v -> caseExpr (Val v) (h:t)
                          otherwise -> Case otherwise (h:t)
  For id x y conds ->   case eval x vars of
                          Val (List l) ->   ListExpr [substitute y [(id, Val item)] | item <- l,
                                                      allTrue [substitute cond [(id, Val item)] | cond <- conds]
                                                      ]
                          ListExpr l ->     ListExpr [substitute y [(id, item)] | item <- l,
                                                      allTrue [substitute cond [(id, item)] | cond <- conds]
                                                      ]
                          Exception e ->    Exception e
                          otherwise ->      For id otherwise y conds
  TakeFor id x y conds n -> case eval x vars of
                          Val (List l) ->   ListExpr (take (fromIntegral n)
                                                     [substitute y [(id, Val item)] | item <- l,
                                                      allTrue [substitute cond [(id, Val item)] | cond <- conds]
                                                      ])
                          ListExpr l ->     ListExpr (take (fromIntegral n)
                                                     [substitute y [(id, item)] | item <- l,
                                                      allTrue [substitute cond [(id, item)] | cond <- conds]
                                                      ])
                          Exception e ->    Exception e
                          otherwise ->      TakeFor id otherwise y conds n
  Range from to step -> case from of
                          Val (NumInt i) -> case to of
                                              Val (NumInt j) -> case step of
                                                                  Val (NumInt k) -> Val $ List [NumInt x | x <- [i, i+k .. j]]
                                                                  Exception e -> Exception e
                                                                  otherwise -> Range from to (eval step vars)
                                              Skip -> case (eval step vars) of
                                                        Val (NumInt k) -> Val $ List [NumInt x | x <- [i, i+k ..]]
                                                        Exception e -> Exception e
                                                        otherwise -> Range from Skip otherwise
                                              Exception e -> Exception e
                                              otherwise -> Range from (eval to vars) step
                          Exception e -> Exception e
                          otherwise -> Range (eval from vars) to step
  FileObj f ->          case eval f vars of
                          Val (Str s) -> Val $ File s
                          otherwise -> FileObj otherwise
  Output x ->           Output (eval (Func (Name "show") [x]) vars)
  FileRead f ->         FileRead (eval f vars)
  FileWrite f x ->      case (eval f vars) of
                          Val (File f) -> case (eval x vars) of
                                            Val (Str s) -> FileWrite (Val (File f)) (Val (Str s))
                                            otherwise -> FileWrite (Val (File f)) otherwise
                          otherwise -> FileWrite otherwise x
  FileAppend f x ->     case (eval f vars) of
                          Val (File f) -> case (eval x vars) of
                                            Val (Str s) -> FileAppend (Val (File f)) (Val (Str s))
                                            otherwise -> FileAppend (Val (File f)) otherwise
                          otherwise -> FileAppend otherwise x
  AtomExpr s v ->       case eval (ListExpr v) vars of
                          Exception e -> Exception e
                          Val (List l) -> Val $ Atom s l
                          otherwise -> AtomExpr s [eval i vars | i <- v]
  otherwise ->          otherwise
 where operation x y f g = case x of
                             Val v -> case y of
                                        Val v -> calc x y f
                                        Exception e -> Exception e
                                        otherwise -> if y' == y 
                                                     then operation x y' f g
                                                     else g x y'
                                                     where y' = eval y vars
                             Exception e -> Exception e
                             otherwise -> if x' == x
                                          then operation x' y f g
                                          else g x' y
                                          where x' = eval x vars
       allTrue [] = True
       allTrue (h:t) = case eval h vars of
                         Val (Bit True) -> allTrue t
                         Exception e -> False
                         Val v -> False
                         otherwise -> if otherwise == h then False else allTrue (otherwise : t)
       
       caseExpr :: Expr -> [(Id, Expr)] -> Expr
       caseExpr check [] = exNoCaseMatch check
       caseExpr check (h:t) = if pattern_match [param] [check]
                              then substitute expr (funcall (zip [param] [check]))
                              else caseExpr check t
                              where param = fst h
                                    expr = snd h
                                    

functionCall f args [] vars = Func f args
functionCall f args (h:t) vars =
  case vardef of
    Val (HFunc (h)) -> if length params > 0 
                       then newcall
                       else case snd $ (varBinding fp (vars !! varHash fp) vars) !! 0 of
                              Func f' args' -> Func f' [eval arg vars | arg <- (args' ++ args)]
                              otherwise -> functionCall f args t vars
    Val (Lambda ids func) -> substitute func (funcall (zip ids args))
    Func f' args' -> Func f' (args' ++ args)
    Val (Atom s l) -> Val (Atom s (l ++ [case arg of
                                           Val v -> v
                                         | arg <- args]))
    AtomExpr s l -> AtomExpr s (l ++ args)
    otherwise -> functionCall f args t vars
  where fp = case vardef of
               Val (HFunc (f')) -> f'
               otherwise -> f
        vardef = snd h
        definition = funcBinding fp args (vars !! varHash fp) vars
        params = fst definition
        expr = snd definition
        newcall = substitute expr (funcall (zip params args))


iolist :: [IO Expr] -> IO [Expr]
iolist [] = do return []
iolist (h:t) = do item <- h
                  rest <- iolist t
                  return (item:rest)

-- ieval: evaluates an expression completely, replacing I/O operations as necessary
ieval :: Expr -> VarDict -> IO Expr
ieval expr vars =
  do result <- subfile (eval expr vars) vars
     case result of
       Val v -> return result
       Exception e -> return result
       Skip -> return result
       Import s t -> return result
       Output p -> do p' <- ieval p vars
                      return $ Output p'
       FileWrite f p -> do p' <- ieval p vars
                           return $ FileWrite f p'
       FileAppend f p -> do p' <- ieval p vars
                            return $ FileAppend f p'
       Func f args -> do args' <- iolist [ieval arg vars | arg <- args]
                         if expr == Func f args' 
                          then return $ Func f args'
                          else ieval (Func f args') vars
       LambdaCall x args -> do args' <- iolist [ieval arg vars | arg <- args]
                               if args == args' 
                                then return $ LambdaCall x args'
                                else ieval (LambdaCall x args') vars
       otherwise -> do if expr == result
                        then return $ exUnableToEval result
                        else do vars' <- case expr of
                                           Def id x y -> do return $ addBinding (id, ([], x)) vars
                                           EagerDef id x y -> do x' <- ieval x vars
                                                                 return $ addBinding (id, ([], x')) vars
                                           otherwise -> do return vars
                                ieval result vars'

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
    ListExpr l -> do list <- iolist [subfile e vars | e <- l]
                     return $ ListExpr list
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
    EagerDef id x y -> do x' <- subfile x vars'
                          y' <- subfile y vars'
                          return $ EagerDef id x' y'
                          where vars' = addBinding (id, ([], eval x vars)) vars
    Def id x y -> do y' <- subfile y vars'
                     return $ Def id x y'
                     where vars' = addBinding (id, ([], x)) vars
    Defun id p x y -> do y' <- subfile y vars
                         return $ Defun id p x y'
                         where vars' = addBinding (id, (p, x))  
                                       (addBinding (id, ([], Val (HFunc id))) 
                                        vars)
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
                     case eval sub vars of
                       Val (File f) -> do exists <- doesFileExist f
                                          case exists of 
                                            True -> do contents <- readFile f
                                                       return $ Val $ Str contents
                                            False -> return $ exFileDNE
                       otherwise -> return $ FileRead f
    Func f args -> do args' <- iolist [subfile arg vars | arg <- args]
                      return $ Func f args'
    LambdaCall x args -> do args' <- iolist [subfile arg vars | arg <- args]
                            return $ LambdaCall x args'
    otherwise -> do return otherwise
