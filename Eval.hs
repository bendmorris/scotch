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
  Import s t ->         Skip
  Take n x ->           case (eval n vars) of
                          Val (NumInt i) -> case x of
                                              Val (List l) -> Val (List (take (fromIntegral i) l))
                                              ListExpr l -> eval (ListExpr (take (fromIntegral i) l)) vars
                                              Range from to step -> case eval (Range from to step) vars of
                                                                      Val (List l) -> Val (List (take (fromIntegral i) l))
                                                                      ListExpr l -> eval (ListExpr (take (fromIntegral i) l)) vars
                                                                      Exception e -> Exception e
                                                                      otherwise -> otherwise
                                              Exception e -> Exception e
                                              otherwise -> case eval (Take n (eval otherwise vars)) vars of
                                                             Val (List l) -> eval (Take n (Val (List l))) vars
                                                             ListExpr l -> eval (Take n (ListExpr l)) vars
                                                             Range f t s -> eval (Take n (Range f t s)) vars
                                                             Exception e -> Exception e
                                                             otherwise -> exTakeNonList
                          Exception e -> Exception e
                          otherwise -> exTakeNonInt
  ListExpr l ->         case (validList l) of
                          Val _ -> case validList [Val item | item <- l'] of
                                     Exception e -> Exception e
                                     otherwise -> Val $ List l'
                                   where l' = [case eval item vars of
                                                 Val r -> r
                                                 Exception e -> Undefined e
                                                 otherwise -> Undefined (show otherwise)
                                               | item <- l]
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
                          Val (Str s) -> Val $ NumInt (read s)
                          Exception e -> Exception e
                          otherwise -> exCantConvert (show otherwise) "integer"
  ToFloat x ->          case (eval x vars) of
                          Val (NumInt i) -> Val $ NumFloat (fromIntegral i)
                          Val (NumFloat f) -> Val $ NumFloat f
                          Val (Str s) -> Val $ NumFloat (read s :: Double)
                          Exception e -> Exception e
                          otherwise -> exCantConvert (show otherwise) "float"
  ToStr x ->            case (eval x vars) of                       
                          Val (Str s) -> Val $ Str s
                          Val (NumFloat f) -> Val $ Str $ showFFloat Nothing f ""
                          Func f args -> exNoMatch f args
                          Exception e -> Exception e
                          otherwise -> Val $ Str (show otherwise)
  Subs n x ->           case (eval x vars) of
                          Val (List l) -> case (eval n vars) of
                                            Val (NumInt n) -> if n >= 0 && 
                                                                 n < (fromIntegral (length l))
                                                              then Val (l !! (fromIntegral n))
                                                              else exNotInList n
                                            Val (List l') ->  eval (ListExpr [Subs (Val i) (Val (List l)) | i <- l']) vars
                                            otherwise ->      exNonNumSubs otherwise
                          Val (Str s) ->  case (eval n vars) of
                                            Val (NumInt n) -> if n >= 0 && 
                                                                 n < (fromIntegral (length s))
                                                              then Val (Str ([s !! (fromIntegral n)]))
                                                              else exNotInList n
                                            Val (List l') ->  eval (ListExpr [Subs (Val i) (Val (Str s)) | i <- l']) vars
                                            otherwise ->      exNonNumSubs otherwise
                          Val (Hash l) -> case eval n vars of
                                            Exception e -> Exception e
                                            otherwise -> case (eval (ToStr otherwise) vars) of
                                                           Val (Str s) ->    eval (Val $ hashMember s l) vars
                                                           Exception e ->    Exception e
                                                           otherwise ->      Exception $ show otherwise
                          Func f args ->  exp
                          otherwise ->    exNotList otherwise
  Add x y ->            eval (calc (eval x vars) (eval y vars) (vadd)) vars
  Sub x y ->            eval (calc (eval x vars) (eval y vars) (vsub)) vars
  Prod x y ->           eval (calc (eval x vars) (eval y vars) (vprod)) vars
  Div x y ->            eval (calc (eval x vars) (eval y vars) (vdiv)) vars
  Mod x y ->            eval (calc (eval x vars) (eval y vars) (vmod)) vars
  Exp x y ->            eval (calc (eval x vars) (eval y vars) (vexp)) vars
  Eq x y ->             eval (calc (eval x vars) (eval y vars) (veq)) vars
  InEq x y ->           case eval (calc (eval x vars) (eval y vars) (veq)) vars of
                          Val (Bit True) -> Val (Bit False)
                          Val (Bit False) -> Val (Bit True)
                          otherwise -> otherwise                    
  Gt x y ->             eval (calc (eval x vars) (eval y vars) (vgt)) vars
  Lt x y ->             eval (calc (eval x vars) (eval y vars) (vlt)) vars         
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
  Def f x y ->          eval y (addBinding (f, ([], x)) vars)
  EagerDef f x y ->     eval y (addBinding (f, ([], eval x vars)) vars)
  Defun f p x y ->      eval y (addBinding (f, (p, x)) 
                                (addBinding(f, ([], Val (HFunc f)))  
                                 vars))
  Defproc f p x y ->    eval y (addBinding (f, (p, Val (Proc x)))
                                (addBinding (f, ([], Val (HFunc f)))
                                 vars))
  Var x ->              eval (snd ((varBinding x (vars !! varHash x) vars) !! 0)) vars
  Func f args ->        case validList evalArgs of
                          Exception e -> Exception e
                          otherwise -> functionCall f evalArgs (varBinding f (vars !! varHash f) vars) vars
                        where evalArgs = [eval arg vars | arg <- args]
  If cond x y ->        case eval (eval cond vars) vars of
                          Val (Bit True) -> eval x vars
                          Val (Bit False) -> eval y vars
                          Exception e -> Exception e
                          otherwise -> exNotBool cond
  Case check (h:t) ->   case (eval check vars) of
                          Exception e -> Exception e
                          otherwise -> caseExpr otherwise (h:t)
  For id x y conds ->   eval (case (eval x vars) of
                                 Val (List l) -> ListExpr (forloop id [Val item | item <- l] y conds)
                                 Val (Str s) -> ListExpr (forloop id [Val (Str [c]) | c <- s] y conds)
                                 Val (Hash h) -> For id (ListExpr [ListExpr [Val (Str (fst l)), Val (snd l)] | e <- h, l <- e]) y conds
                                 Val v -> ListExpr (forloop id [Val v] y conds)
                                 otherwise -> Exception (show x)) vars
  Range from to step -> case (eval from vars) of
                          Val (NumInt i) -> case (eval to vars) of
                                              Val (NumInt j) -> case (eval step vars) of
                                                                  Val (NumInt k) -> Val $ List [NumInt x | x <- [i, i+k .. j]]
                                                                  Exception e -> Exception e
                                                                  otherwise -> exNonIntInRange
                                              Skip -> case (eval step vars) of
                                                        Val (NumInt k) -> Val $ List [NumInt x | x <- [i, i+k ..]]
                                                        Exception e -> Exception e
                                                        otherwise -> exNonIntInRange
                                              Exception e -> Exception e
                                              otherwise -> exNonIntInRange
                          Exception e -> Exception e
                          otherwise -> exNonIntInRange
  FileObj f ->          case eval f vars of
                          Val (Str s) -> Val $ File s
                          otherwise -> exNonStrFilename
  Output x ->           Output (eval (Func (Name "show") [x]) vars)
  FileWrite f x ->      case (eval f vars) of
                          Val (File f) -> case (eval x vars) of
                                            Val (Str s) -> FileWrite (Val (File f)) (Val (Str s))
                                            otherwise -> exWriteNonString otherwise
                          otherwise -> exWriteNonFile otherwise
  FileAppend f x ->     case (eval f vars) of
                          Val (File f) -> case (eval x vars) of
                                            Val (Str s) -> FileAppend (Val (File f)) (Val (Str s))
                                            otherwise -> exWriteNonString otherwise
                          otherwise -> exWriteNonFile otherwise
  AtomExpr s v ->       case validList atomList of
                          Exception e -> Exception e
                          otherwise -> Val $ Atom s [case result of
                                                       Exception e -> Undefined e
                                                       Val r -> r
                                                       otherwise -> Undefined $ show otherwise
                                                     | result <- atomList ]
                        where atomList = [eval v' vars | v' <- v]
  otherwise ->          otherwise
 where forloop :: Id -> [Expr] -> Expr -> [Expr] -> [Expr]
       forloop id [] y conds = []
       forloop id (h:t) y conds = [i | i <- ([e | e <- [eval (substitute y [(id,h)]) vars],
                                             allTrue [eval (substitute cond [(id,h)]) vars | cond <- conds]] 
                                             ++ (forloop id t y conds))]
       allTrue [] = True
       allTrue (h:t) = case h of
                         Val (Bit True) -> allTrue t
                         otherwise -> False
       
       caseExpr :: Expr -> [(Id, Expr)] -> Expr
       caseExpr check [] = exNoCaseMatch check
       caseExpr check (h:t) = if pattern_match [param] [check]
                              then eval (substitute expr (funcall (zip [param] [check]))) vars
                              else caseExpr check t
                              where param = fst h
                                    expr = snd h
                                    

functionCall f args [] vars = Func f args
functionCall f args (h:t) vars =
  case vardef of
    Val (HFunc (h)) -> if length params > 0 
                       then checkTailRecursion fp args definition newcall vars
                       else case snd $ (varBinding fp (vars !! varHash fp) vars) !! 0 of
                              Func f' args' -> eval (Func f' [eval arg vars | arg <- (args' ++ args)]) vars
                              otherwise -> functionCall f args t vars
    Val (Lambda ids func) -> eval (substitute func (funcall (zip ids args))) vars
    Func f' args' -> eval (Func f' [eval arg vars | arg <- (args' ++ args)]) vars
    Val (Atom s l) -> Val (Atom s (l ++ [case arg of
                                           Val v -> v
                                         | arg <- args]))
    AtomExpr s l -> eval (AtomExpr s (l ++ args)) vars
    otherwise -> functionCall f args t vars
  where fp = case vardef of
               Val (HFunc (f')) -> f'
               otherwise -> f
        vardef = snd h
        definition = funcBinding fp args (vars !! varHash fp) vars
        params = fst definition
        expr = snd definition
        newcall = eval (substitute expr (funcall (zip params args))) vars

tailcall definition f args args' vars = 
  if definition' == definition 
  then tailcall definition f [eval (substitute (args' !! n) (funcall (zip params args))) vars
                              | n <- [0 .. (length args') - 1]] 
                              args' vars
  else eval (substitute (snd definition') (funcall (zip (fst definition') args))) vars
  where definition' = funcBinding f args (vars !! varHash f) vars
        params = fst definition

checkTailRecursion :: Id -> [Expr] -> Call -> Expr -> VarDict -> Expr
checkTailRecursion f args definition newcall vars =
  case snd definition of
    Func f' args' -> if f == f' 
                     then tailcall definition f args args' vars
                     else newcall
    otherwise -> newcall


iolist :: [IO Expr] -> IO [Expr]
iolist [] = do return []
iolist (h:t) = do item <- h
                  rest <- iolist t
                  return (item:rest)
                  
-- ieval: evaluates an expression completely, replacing I/O operations as necessary
ieval :: Expr -> VarDict -> IO Expr
ieval expr vars =
  do subbed <- subfile expr vars
     let result = eval subbed vars
     case result of
       Val v -> return result
       Exception e -> return result
       Skip -> return result
       Output p -> return result
       FileWrite f p -> return result
       FileAppend f p -> return result
       Func f args -> do args' <- iolist [subfile arg vars | arg <- args]
                         if (eval (Func f args') vars) == result
                          then return result
                          else ieval result vars
       otherwise -> do let vars' = case expr of
                                     Def id x y -> addBinding (id, ([], x)) vars
                                     EagerDef id x y -> addBinding (id, ([], x)) vars
                                     otherwise -> vars
                       ieval result vars'

-- subfile: substitutes values for delayed I/O operations
subfile :: Expr -> VarDict -> IO Expr
subfile exp vars =
  case exp of
    ToInt x -> do x' <- subfile x vars
                  return $ ToInt x'
    ToFloat x -> do x' <- subfile x vars
                    return $ ToFloat x'
    ToStr x -> do x' <- subfile x vars
                  return $ ToStr x'
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
                       otherwise -> do return $ exInvalidFile
    Func f args -> do args' <- iolist [subfile arg vars | arg <- args]
                      return $ Func f args'
    otherwise -> do return otherwise
