{-  This file is part of Scotch.

    Scotch is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Scotch is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Scotch.  If not, see <http://www.gnu.org/licenses/>.
-}

module Scotch.Eval.Substitute where

import Scotch.Types.Types
import Scotch.Types.Bindings
import Scotch.Types.Hash
import Scotch.Eval.Calc
import Data.List

-- if expression x should be rewritten, return the rewritten expression;
-- otherwise, returns an InvalidValue
inparams :: Expr -> [Binding] -> Bool -> Expr
inparams x [] fromDict = Val (InvalidValue)
inparams x (h:t) fromDict = 
  if fst match 
  then subDefs (snd h) (snd match) False
  else inparams x t fromDict
  where match = patternMatch x (fst h) fromDict

nameMatch x y = x == y || isSuffixOf ("." ++ y) ("." ++ x)

-- check if expression x matches definition y
patternMatch :: Expr -> Expr -> Bool -> (Bool, [Binding])
patternMatch x y fromDict =
  case (x, y) of
    (_, Var v) ->               case fromDict of
                                  True -> (True, if x == y 
                                                 then []
                                                 else [(y, x)])
                                  False -> case x of
                                             Var v2 -> (nameMatch v v2, [])
                                             otherwise -> (False, [])
    (Call (Var v1) args1, 
     Call (Var v2) args2) -> 
                                if length args1 == length args2
                                then trySubs 
                                     [patternMatch' (args1 !! n) (args2 !! n)
                                      | n <- [0 .. (length args1) - 1]]
                                else (False, [])
    (_, Concat (Var v1) 
               (Var v2)) ->     case x of
                                  ListExpr l ->     if length l > 0 
                                                    then (True, [(Var v1, head l),
                                                                 (Var v2, ListExpr (tail l))])
                                                    else (False, [])
                                  Val (List l) ->   if length l > 0 
                                                    then (True, [(Var v1, Val (head l)),
                                                                 (Var v2, Val (List (tail l)))])
                                                    else (False, [])
                                  Val (Str l) ->    if length l > 0 
                                                    then (True, [(Var v1, Val (Str [head l])),
                                                                 (Var v2, Val (Str (tail l)))])
                                                    else (False, [])
                                  otherwise -> (False, [])
    (Add a b, Add c d) ->       trySubs [patternMatch' a c, patternMatch' b d]
    (Sub a b, Sub c d) ->       trySubs [patternMatch' a c, patternMatch' b d]
    (Prod a b, Prod c d) ->     trySubs [patternMatch' a c, patternMatch' b d]
    (Div a b, Div c d) ->       trySubs [patternMatch' a c, patternMatch' b d]
    (Exp a b, Exp c d) ->       trySubs [patternMatch' a c, patternMatch' b d]
    otherwise ->                if veq False x y == Val (Bit True)
                                then (True, []) else (False, [])
  where trySubs exprs = if all ((==) True) [fst expr | expr <- exprs]
                        then (True, foldl (++) [] [snd expr | expr <- exprs])
                        else (False, [])
        patternMatch' a b = patternMatch a b fromDict
                 
subDefs :: Expr -> [Binding] -> Bool -> Expr
subDefs exp [] fromDict = exp
subDefs exp params fromDict = substitute exp (makeVarDict params) fromDict

substitute :: Expr -> VarDict -> Bool -> Expr
substitute exp [] fromDict = exp
substitute exp params fromDict =
  case inparams exp (params !! (exprHash exp)) fromDict of
    Val (InvalidValue) -> 
      case exp of
        Call id args -> Call (substitute' id) [substitute' arg | arg <- args]
        Val (Proc p) -> Val (Proc ([substitute' e | e <- p]))
        Val (Lambda ids expr) -> Val (Lambda ids (substitute' expr))
        Val (Thread e) -> Val (Thread (substitute' e))
        Take n x -> Take (substitute' n) (substitute' x)
        ToInt x -> ToInt (substitute' x)
        ToFloat x -> ToFloat (substitute' x)
        ToStr x -> ToStr (substitute' x)
        ToList l -> ToList (substitute' l)
        ListExpr l -> ListExpr [substitute' e | e <- l]    
        HashExpr l -> HashExpr [(substitute' (fst kv), substitute' (snd kv)) | kv <- l]
        Subs n x -> Subs (substitute' n) (substitute' x)
        Concat x y -> Concat (substitute' x) (substitute' y)
        Add x y -> Add (substitute' x) (substitute' y)
        Sub x y -> Sub (substitute' x) (substitute' y)
        Prod x y -> Prod (substitute' x) (substitute' y)
        Div x y -> Div (substitute' x) (substitute' y)
        Mod x y -> Mod (substitute' x) (substitute' y)
        Exp x y -> Exp (substitute' x) (substitute' y)
        Eq x y -> Eq (substitute' x) (substitute' y)
        InEq x y -> InEq (substitute' x) (substitute' y)
        Gt x y -> Gt (substitute' x) (substitute' y)
        Lt x y -> Lt (substitute' x) (substitute' y)
        And x y -> And (substitute' x) (substitute' y)
        Or x y -> Or (substitute' x) (substitute' y)
        Not x -> Not (substitute' x)
        Def id x y -> Def id (substitute' x) (substitute' y)
        EagerDef id x y -> EagerDef id (substitute' x) (substitute' y)
        If x y z -> If (substitute' x) (substitute' y) (substitute' z)
        Case c opts -> Case (substitute' c) [(fst opt, substitute' (snd opt)) | opt <- opts]
        For id x y z -> For id (substitute' x) (substitute' y) [substitute' i | i <- z]
        Range x y z -> Range (substitute' x) (substitute' y) (substitute' z)
        Output x -> Output (substitute' x)
        FileObj x -> FileObj (substitute' x)
        FileRead x -> FileRead (substitute' x)
        FileWrite f x -> FileWrite (substitute' f) (substitute' x)
        FileAppend f x -> FileAppend (substitute' f) (substitute' x)
        EvalExpr x -> EvalExpr (substitute' x)
        otherwise -> otherwise
      where substitute' x = substitute x params fromDict
    otherwise -> otherwise
