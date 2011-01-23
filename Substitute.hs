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

module Substitute where

import Types

inparams :: Id -> [(Id, Expr)] -> Expr
inparams x [] = Skip
inparams x (h:t) = if x == (fst h) then snd h else inparams x t

inparamsid :: Id -> [(Id, Expr)] -> (Value, (Id, [Expr]))
inparamsid x [] = (Null, (x, []))
inparamsid x (h:t) = if x == (fst h) 
                     then case snd h of
                            Var v -> (Null, (v, []))
                            Val (HFunc h) -> (Null, (h, []))
                            Val (Lambda ids expr) -> (Lambda ids expr, (fst h, []))
                            Func f args -> (Null, (f, args))
                            otherwise -> inparamsid x t
                     else inparamsid x t

substitute :: Expr -> [(Id, Expr)] -> Expr
substitute exp params =
  case exp of
    Var x -> if newname == Skip then Var x else newname
             where newname = inparams x params
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
    AtomExpr s e -> AtomExpr s [substitute i params | i <- e]
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
    EagerDef id x y -> EagerDef id (substitute x params) (substitute y params)
    Def id x y -> Def id (substitute x params) (substitute y params)
    Defun id p x y -> Defun id p (substitute x params) (substitute y params)
    If x y z -> If (substitute x params) (substitute y params) (substitute z params)
    Case c opts -> Case (substitute c params) [(fst opt, substitute (snd opt) params) | opt <- opts]
    For id x y z -> For id (substitute x params) (substitute y params) [substitute i params | i <- z]
    Range start stop step -> Range (substitute start params) (substitute stop params) (substitute step params)
    Func f args -> case inparamsid f params of
                     (Null, (f', [])) -> Func f' [substitute arg params | arg <- args]
                     (Null, (f', otherargs)) -> Func f' [substitute arg params | arg <- otherargs ++ args]
                     (Lambda ids expr, _) -> substitute (LambdaCall (Lambda ids (substitute expr params)) args) params
    LambdaCall (Lambda ids expr) args -> substitute (Defun (Name "lambda") ids (substitute expr params) (
                                                      Def (Name "lambda") (Val (HFunc (Name "lambda"))) (
                                                       Func (Name "lambda") args
                                                       )
                                                      )
                                                     ) params
    Output x -> Output (substitute x params)
    FileObj x -> FileObj (substitute x params)
    FileRead x -> FileRead (substitute x params)
    FileWrite f x -> FileWrite (substitute f params) (substitute x params)
    FileAppend f x -> FileAppend (substitute f params) (substitute x params)
    otherwise -> otherwise
