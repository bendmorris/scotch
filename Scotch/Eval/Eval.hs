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

module Scotch.Eval.Eval where

import Data.List
import Numeric
import System.Directory
import Scotch.Types.Types
import Scotch.Types.Exceptions
import Scotch.Types.Bindings
import Scotch.Types.Hash
import Scotch.Eval.Calc
import Scotch.Eval.Substitute
import Scotch.Parse.Parse as Parse


-- eval: computes the value of an expression as far as possible
eval :: Expr -> VarDict -> Bool -> Expr
eval exp [] strict = eval exp emptyHash strict
eval oexp vars strict = case exp of
  Var id ->             if length (qualHashDict id) > 0
                        then Val $ Hash $ makeHash strHash (qualHashDict id) emptyHash
                        else if length (qualHashDict ("local." ++ id)) > 0
                             then Val $ Hash $ makeHash strHash (qualHashDict ("local." ++ id)) emptyHash
                             else Var id
                        where qualHashDict id = [([show (fst v) !! n | n <- [length id + 1 .. length (show (fst v)) - 1]], 
                                                 snd v) 
                                                | i <- vars, v <- i,
                                                  (case fst v of
                                                     Var id' -> isPrefixOf (id ++ ".") id'
                                                     Call (Var id') _ -> isPrefixOf (id ++ ".") id'
                                                     otherwise -> False)]
  Call (Call id args) args' -> eval' $ Call id (args ++ args')
  Call (Var id) args -> Call (Var id) [eval' arg | arg <- args]
  Call (Val (Lambda ids expr)) args ->
                        if length ids == length args
                        then substitute expr (zip [Var id | id <- ids] args)
                        else exp
  Call x args ->        Call (eval' x) args
  EvalExpr x ->         case eval' x of
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
                                              ListExpr l -> eval' (ListExpr (take i' l))
                                              Range from to step -> case eval' (Range from to step) of
                                                                      Val (List l) -> Val (List (take i' l))
                                                                      ListExpr l -> eval' (ListExpr (take i' l))
                                                                      Exception e -> Exception e
                                                                      otherwise -> otherwise
                                              Val (Str s) -> Val $ Str $ take i' s
                                              For id x y conds -> TakeFor id x y conds i
                                              Exception e -> Exception e
                                              Add (ListExpr l) (y) -> if length t == i'
                                                                      then ListExpr t
                                                                      else Take n (eval' x)
                                                                      where t = take i' l
                                              Add (Val (List l)) (y) -> if length t == i'
                                                                        then Val $ List t
                                                                        else Take n (eval' x)
                                                                        where t = take i' l
                                              otherwise -> Take n (eval' x)
                                            where i' = fromIntegral i
                          Exception e -> Exception e
                          otherwise -> Take (eval' otherwise) x
  ListExpr l ->         case (validList l) of
                          Val _ -> case validList l'' of
                                     Exception e -> Exception e
                                     otherwise -> if computableList l''
                                                  then Val $ List l'
                                                  else ListExpr [eval' i | i <- l]
                                   where l' = [case eval' item of
                                                 Val r -> r
                                                 Exception e -> Undefined e
                                                 otherwise -> InvalidValue
                                               | item <- l]
                                         l'' = [Val item | item <- l']
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
                          otherwise -> ToStr $ eval' otherwise
  ToList x ->           case eval' x of
                          Val (List l) -> Val $ List l
                          ListExpr l -> ListExpr l
                          Val (Str s) -> Val $ List [Str [c] | c <- s]
                          Val (Hash h) -> ListExpr [ListExpr [Val (Str (fst l)), snd l] | e <- h, l <- e]
                          Val (File f) -> Call (Var "std.lib.split") [FileRead (Val (File f)), Val (Str "\n")]
                          FileObj f -> Call (Var "std.lib.split") [FileRead (f), Val (Str "\n")]
                          Exception e -> Exception e
                          Val v -> Val (List [v])
                          otherwise -> ToList $ eval' otherwise
  Subs n x ->           case x of
                          Val (List l) -> case eval' n of
                                            Val (NumInt n) -> if n >= 0
                                                              then Val (l !! (fromIntegral n))
                                                              else Val (l !! ((length l) + (fromIntegral n)))
                                            Val (List l') ->  ListExpr [Subs (Val i) (Val (List l)) | i <- l']
                                            otherwise ->      exNonNumSubs otherwise
                          ListExpr l ->   case eval' n of
                                            Val (NumInt n) -> if n >= 0
                                                              then l !! (fromIntegral n)
                                                              else l !! ((length l) + (fromIntegral n))
                                            Val (List l') ->  ListExpr [Subs (Val i) (ListExpr l) | i <- l']
                                            otherwise ->      exNonNumSubs otherwise
                          Val (Str s) ->  case eval' n of
                                            Val (NumInt n) -> if n >= 0
                                                              then Val (Str ([s !! (fromIntegral n)]))
                                                              else Val (Str ([s !! ((length s) + (fromIntegral n))]))
                                            Val (List l') ->  ListExpr [Subs (Val i) (Val (Str s)) | i <- l']
                                            otherwise ->      exNonNumSubs otherwise
                          Val (Hash l) -> case eval' n of
                                            Exception e -> Exception e
                                            otherwise -> case eval' (ToStr otherwise) of
                                                           Val (Str s) ->    case hashMember strHash s l of
                                                                               Just x -> x
                                                                               Nothing -> exNotInHash s
                                                           Exception e ->    Exception e
                                                           otherwise ->      Subs otherwise (Val (Hash l))
                          Call (Var f) args ->  case eval' n of
                                                  Val (NumInt n) -> if n >= 0
                                                                    then eval' $ Subs (Val (NumInt n)) (eval' (Take (Val (NumInt ((fromIntegral n) + 1))) (Call (Var f) args)))
                                                                    else Subs (Val (NumInt n)) (eval' x)
                                                  Val (List l') ->  ListExpr [Subs (Val i) f' | i <- l']
                                                                    where f' = (Call (Var f) args)
                                                  otherwise ->      Subs otherwise (eval' x)
                          otherwise ->    Subs n (eval' otherwise)
  Concat x y ->         eval' (Add (ToList x) (ToList y))
  Add x y ->            case x of
                          Exception e ->    Exception e
                          ListExpr l ->     case y of
                                              Exception e -> Exception e
                                              ListExpr l' -> ListExpr (l ++ l')
                                              Val v -> Add (eval' x) y
                                              Add a b -> Add (eval' (Add x a)) b
                                              otherwise -> Add x (eval' y)
                          Val v ->          case y of
                                              Exception e -> Exception e
                                              Val v -> vadd strict x y
                                              otherwise -> Add x (eval' y)
                          otherwise -> Add (eval' x) y
  Sub x y ->            operation x y vsub Sub
  Prod x y ->           operation x y vprod Prod
  Div x y ->            operation x y vdiv Div
  Mod x y ->            operation x y vmod Mod
  Exp x y ->            operation x y vexp Exp
  Eq x y ->             operation x y veq Eq
  InEq x y ->           eval' (Prod (operation x y veq Eq) (Val (NumInt (-1))))
  Gt x y ->             operation x y vgt Gt
  Lt x y ->             operation x y vlt Lt
  And x y ->            case eval' x of
                          Val (Bit True) -> case eval' y of
                                              Val (Bit True) -> Val (Bit True)
                                              Val (Bit False) -> Val (Bit False)
                                              otherwise -> err
                          Val (Bit False) -> Val (Bit False)
                          otherwise -> err
                        where err = exTypeMismatch (eval' x) (eval' y) "and"
  Or x y ->             case eval' x of
                          Val (Bit True) -> Val (Bit True)                                            
                          Val (Bit False) -> case eval' y of
                                               Val (Bit b) -> Val (Bit b)
                                               otherwise -> err
                          otherwise -> err
                        where err = exTypeMismatch (eval' x) (eval' y) "or"
  Not x ->              case eval' x of
                          Exception s -> Exception s
                          Val r -> case r of
                                     Bit b -> Val (Bit (not b))
                                     otherwise -> exNotBool otherwise
                          otherwise -> Not otherwise
  Def f x y ->          eval' (substitute y [(f, x)])
  EagerDef f x y ->     case eval' x of
                          Exception e -> Exception e
                          Val v -> eval' (substitute y [(f, Val v)])
                          otherwise -> EagerDef f (eval' otherwise) y
  If cond x y ->        case eval' cond of
                          Val (Bit True) -> x
                          Val (Bit False) -> y
                          Exception e -> Exception e
                          otherwise -> If otherwise x y
  Case check cases ->   case check of
                          Exception e -> Exception e
                          otherwise -> rewrite check [(Eq (check) (fst thiscase), snd thiscase) | thiscase <- cases]
  For id x y conds ->   case eval' x of
                          Val (List l) ->   ListExpr [substitute y [(Var id, Val item)] | item <- l,
                                                      allTrue [substitute cond [(Var id, Val item)] | cond <- conds]
                                                      ]
                          ListExpr l ->     ListExpr [substitute y [(Var id, item)] | item <- l,
                                                      allTrue [substitute cond [(Var id, item)] | cond <- conds]
                                                      ]
                          Exception e ->    Exception e
                          otherwise ->      For id otherwise y conds
  TakeFor id x y conds n -> case eval' x of
                          Val (List l) ->   ListExpr (take (fromIntegral n)
                                                     [substitute y [(Var id, Val item)] | item <- l,
                                                      allTrue [substitute cond [(Var id, Val item)] | cond <- conds]
                                                      ])
                          ListExpr l ->     ListExpr (take (fromIntegral n)
                                                     [substitute y [(Var id, item)] | item <- l,
                                                      allTrue [substitute cond [(Var id, item)] | cond <- conds]
                                                      ])
                          Exception e ->    Exception e
                          otherwise ->      TakeFor id otherwise y conds n
  Range from to step -> case from of
                          Val (NumInt i) -> case to of
                                              Val (NumInt j) -> case step of
                                                                  Val (NumInt k) -> Val $ List [NumInt x | x <- [i, i+k .. j]]
                                                                  Exception e -> Exception e
                                                                  otherwise -> Range from to (eval' step)
                                              Skip -> case (eval' step) of
                                                        Val (NumInt k) -> Val $ List [NumInt x | x <- [i, i+k ..]]
                                                        Exception e -> Exception e
                                                        otherwise -> Range from Skip otherwise
                                              Exception e -> Exception e
                                              otherwise -> Range from (eval' to) step
                          Exception e -> Exception e
                          otherwise -> Range (eval' from) to step
  FileObj f ->          case eval' f of
                          Val (Str s) -> Val $ File s
                          otherwise -> FileObj otherwise
  Output x ->           Output (eval' (Call (Var "show") [x]))
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
  otherwise ->          otherwise
 where operation x y f g = case x of
                             Val v -> case y of
                                        Val v -> calc x y f strict
                                        Exception e -> Exception e
                                        otherwise -> if y' == y 
                                                     then operation x y' f g
                                                     else g x y'
                                                     where y' = eval' y
                             Exception e -> Exception e
                             otherwise -> if x' == x
                                          then operation x' y f g
                                          else g x' y
                                          where x' = eval' x
       allTrue [] = True
       allTrue (h:t) = case eval' h of
                         Val (Bit True) -> allTrue t
                         Exception e -> False
                         Val v -> False
                         otherwise -> if otherwise == h then False else allTrue (otherwise : t)
       
       exp = if nexp == oexp
             then oexp
             else eval' nexp
             where nexp = rewrite oexp (vars !! exprHash oexp)
       eval' expr = eval expr vars strict
                                    

iolist :: [IO Expr] -> IO [Expr]
iolist [] = do return []
iolist (h:t) = do item <- h
                  rest <- iolist t
                  return (item:rest)

-- ieval: evaluates an expression completely, replacing I/O operations as necessary
ieval :: Expr -> VarDict -> Bool -> Maybe Expr -> IO Expr
ieval expr vars strict last =
  do result <- subfile (eval expr vars strict) vars
     if Just result == last
      then return result
      else do --putStrLn $ show result
              vars' <- case expr of
                         Def id x y -> do return $ makeVarDict [(id, x)] vars
                         EagerDef id x y -> do x' <- ieval x vars strict (Just result)
                                               return $ makeVarDict [(id, x')] vars
                         otherwise -> do return vars
              ieval result vars' strict (Just result)

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
