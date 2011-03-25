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
import Data.List

-- if expression x should be rewritten, return the rewritten expression;
-- otherwise, returns an InvalidValue
inparams :: Expr -> [Binding] -> Expr
inparams x [] = Val (InvalidValue)
inparams x (h:t) = if fst match 
                   then subDefs (snd h) (snd match)
                   else inparams x t
                   where match = case fst h of
                                   Var id args -> case x of 
                                                    Var id' args' -> if nameMatch id id' && 
                                                                        length args <= length args'
                                                                     then (True, zip args args')
                                                                     else (False, [])
                                                    otherwise -> (False, [])
                                   otherwise -> patternMatch x (fst h)

nameMatch x y = x == y || isSuffixOf ("." ++ y) ("." ++ x)

-- check if expression x matches definition y
patternMatch :: Expr -> Expr -> (Bool, [Binding])
patternMatch x y =
  case (x, y) of
    (_, Var v []) ->            (True, [(y, x)])
    (Var v1 args1, Var v2 args2) -> if length args1 == length args2
                                    then trySubs 
                                         [patternMatch (args1 !! n) (args2 !! n)
                                          | n <- [0 .. (length args1) - 1]]
                                    else (False, [])
    {-(_, Add (Var v1 []) (Var v2 [])) ->
                                case x of
                                  ListExpr l ->     if length l > 0 
                                                    then (True, [(Var v1 [], head l),
                                                                 (Var v2 [], ListExpr (tail l))])
                                                    else (False, [])
                                  Val (List l) ->   if length l > 0 
                                                    then (True, [(Var v1 [], Val (head l)),
                                                                 (Var v2 [], Val (List (tail l)))])
                                                    else (False, [])
                                  otherwise -> (False, [])-}
    (Add a b, Add c d) ->       trySubs [patternMatch a c, patternMatch b d]
    (Sub a b, Sub c d) ->       trySubs [patternMatch a c, patternMatch b d]
    (Prod a b, Prod c d) ->     trySubs [patternMatch a c, patternMatch b d]
    (Div a b, Div c d) ->       trySubs [patternMatch a c, patternMatch b d]
    (Exp a b, Exp c d) ->       trySubs [patternMatch a c, patternMatch b d]
    otherwise ->                if x == y then (True, []) else (False, [])
  where trySubs exprs = if all ((==) True) [fst expr | expr <- exprs]
                        then (True, foldl (++) [] [snd expr | expr <- exprs])
                        else (False, [])
                 
subDefs :: Expr -> [Binding] -> Expr
subDefs exp [] = exp
subDefs exp params = substitute exp (makeVarDict params)

substitute :: Expr -> VarDict -> Expr
substitute exp [] = exp
substitute exp params =
  case inparams exp (params !! (exprHash exp)) of
    Val (InvalidValue) -> 
      case exp of
        Var id args -> Var id [substitute arg params | arg <- args]
        Val (Proc p) -> Val (Proc ([substitute e params | e <- p]))
        Val (Lambda ids expr) -> Val (Lambda ids (substitute expr params))
        Val (Thread e) -> Val (Thread (substitute e params))
        Take n x -> Take (substitute n params) (substitute x params)
        ToInt x -> ToInt (substitute x params)
        ToFloat x -> ToFloat (substitute x params)
        ToStr x -> ToStr (substitute x params)
        ToList l -> ToList (substitute l params)
        ListExpr l -> ListExpr [substitute e params | e <- l]    
        HashExpr l -> HashExpr [(substitute (fst kv) params, substitute (snd kv) params) | kv <- l]
        Subs n x -> Subs (substitute n params) (substitute x params)
        Add x y -> Add (substitute x params) (substitute y params)
        Sub x y -> Sub (substitute x params) (substitute y params)
        Prod x y -> Prod (substitute x params) (substitute y params)
        Div x y -> Div (substitute x params) (substitute y params)
        Mod x y -> Mod (substitute x params) (substitute y params)
        Exp x y -> Exp (substitute x params) (substitute y params)
        Eq x y -> Eq (substitute x params) (substitute y params)
        InEq x y -> InEq (substitute x params) (substitute y params)
        Gt x y -> Gt (substitute x params) (substitute y params)
        Lt x y -> Lt (substitute x params) (substitute y params)
        And x y -> And (substitute x params) (substitute y params)
        Or x y -> Or (substitute x params) (substitute y params)
        Not x -> Not (substitute x params)
        Def id x y -> Def id (substitute x params) (substitute y params)
        EagerDef id x y -> EagerDef id (substitute x params) (substitute y params)
        If x y z -> If (substitute x params) (substitute y params) (substitute z params)
        Case c opts -> Case (substitute c params) [(fst opt, substitute (snd opt) params) | opt <- opts]
        For id x y z -> For id (substitute x params) (substitute y params) [substitute i params | i <- z]
        Range start stop step -> Range (substitute start params) (substitute stop params) (substitute step params)
        Output x -> Output (substitute x params)
        FileObj x -> FileObj (substitute x params)
        FileRead x -> FileRead (substitute x params)
        FileWrite f x -> FileWrite (substitute f params) (substitute x params)
        FileAppend f x -> FileAppend (substitute f params) (substitute x params)
        EvalExpr x -> EvalExpr (substitute x params)
        otherwise -> otherwise
    otherwise -> otherwise
