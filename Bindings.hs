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

module Bindings where

import Data.List
import Calc
import Types
import Hash

-- a list of identifiers (empty list for variables) and an expression containing them
type Call = ([Id], Expr)
-- binds an ID to a Call
type Binding = (Id, Call)
type VarDict = [[Binding]]

stripName :: Id -> String
stripName (Name n) = n
stripName n = ""

varName' :: String -> Int -> String
varName' s 0 = s
varName' s n = if s !! n == '.'
               then snd $ splitAt (n+1) s
               else varName' s (n-1)
varName "" = ""
varName s = varName' s ((length s) - 1)

varHash :: Id -> Int
varHash b = case b of
              Name n -> hashLoc $ varName n
              otherwise -> hashLoc $ show $ b

newBindingHash :: [Binding] -> VarDict -> VarDict
newBindingHash [] hash = hash
newBindingHash (h:t) hash = addBinding (h) (newBindingHash t hash)
-- adds a Binding to a VarDict, removing bindings that are now irrelevant
addBinding :: Binding -> VarDict -> VarDict
addBinding binding vars = [vars !! n | n <- [0 .. (hash - 1)]]
                          ++ [binding : (removeBindingFrom binding (vars !! hash))] ++
                          [vars !! n | n <- [(hash + 1) .. (hashSize - 1)]]
                          where hash = varHash (fst binding)
                    
samePattern [] [] = True
samePattern (h:t) (h':t') = case (h, h') of
                              (Name a, Name b) -> samePattern t t'
                              (Split a b, Split c d) -> samePattern t t'
                              (Pattern a, Pattern b) -> if a == b 
                                                        then samePattern t t'
                                                        else False
                              otherwise -> False
removeBindingFrom :: Binding -> [Binding] -> [Binding]
removeBindingFrom _ [] = []
removeBindingFrom binding (h:t) = if fst h == fst binding &&
                                     length (fst (snd h)) ==
                                     length (fst (snd binding)) &&
                                     samePattern (fst (snd h)) (fst (snd binding))
                                  then removeBindingFrom binding t
                                  else h: (removeBindingFrom binding t)
addBindings (h:t) vars = addBinding h (addBindings t vars)
addBindings [] vars = vars


nameSplit (Name n) = n

varBinding :: Id -> [Binding] -> VarDict -> [Call]
varBinding x [] vars = [([], Exception ("Undefined variable " ++ show x))]
varBinding x (h:t) vars = if ((fst h) == x || isSuffixOf ("." ++ nameSplit x) ("." ++ nameSplit (fst h))) && 
                              length (fst (snd h)) == 0 && 
                              snd (snd h) /= Var (fst h)
                           then case snd (snd h) of
                                  Var v -> if v == x 
                                           then varBinding x t vars
                                           else varBinding v (vars !! varHash v) vars
                                  otherwise -> snd h : varBinding x t vars
                           else varBinding x t vars
                                                      
funcBinding :: Id -> [Expr] -> [Binding] -> VarDict -> Call
funcBinding x args [] vars = ([], Exception ("Function " ++ (show x) ++ " " ++ show args ++ " doesn't match any existing pattern."))
funcBinding x args (h:t) vars = 
  if (id == x || isSuffixOf ("." ++ nameSplit x) ("." ++ nameSplit id)) &&
     length args == length params &&
     pattern_match params args
  then case validList args of
         Val _ -> binding
         Exception e -> ([], Exception e)
  else funcBinding x args t vars
  where (id, params, expr) = (fst h, fst binding, snd binding)
        binding = snd h

pattern_match :: [Id] -> [Expr] -> Bool
pattern_match [] [] = True
pattern_match (a:b) (c:d) = 
  case a of
    Name n -> pattern_match b d
    Split x y -> case c of
                   Val (List l) -> pattern_match b d
                   Val (Str l) -> pattern_match b d
                   otherwise -> False
    AtomMatch x y -> case c of
                       Val (Atom x' y') -> if length y == length y' 
                                           then if x' == x 
                                                then pattern_match (y ++ b) ([Val i | i <- y'] ++ d)
                                                else False
                                           else False
                       otherwise -> False
    Pattern v -> if c == Val v 
                 then pattern_match b d
                 else case (c, v) of 
                        (Val (List []), Str "") -> pattern_match b d
                        (Val (Str ""), List []) -> pattern_match b d
                        otherwise -> False
                        
-- funcall: list of (ID parameter, expression argument)
funcall :: [(Id, Expr)] -> [(Id, Expr)]
funcall [] = []
funcall (h:t) = 
  case param of
     Name n -> h : funcall t
     Split x y -> case arg of
                    Val (List l) -> if length l > 0 then (Name x, Val (head l)) :
                                                         (Name y, Val (List (tail l))) :
                                                         funcall t
                                                    else [(Name x, Exception "Can't split empty list")]
                    Val (Str l) -> if length l > 0 then (Name x, Val (Str [head l])) :
                                                        (Name y, Val (Str (tail l))) :
                                                        funcall t
                                                   else [(Name x, Exception "Can't split empty string")]
     AtomMatch x y -> case arg of 
                        Val (Atom x' y') -> funcall ((zip y [Val i | i <- y']) ++ t)
     Pattern _ -> funcall t
     where param = fst h
           arg = snd h

permanentDefs id p x s a = case s of
                             Defun id' p' x' s' -> permanentDefs id' p' x' s' (newDef ++ a)
                             Skip -> newDef ++ a
                             otherwise -> []
                           where newDef = [(localId id, (p, x)), (localId id, ([], Val (HFunc (localId id))))]                                 
newDefs (Defun id p x s) = permanentDefs id p x s []

localId id = (Name ("local." ++ stripLocal (stripName id)))
stripLocal s = if isPrefixOf "local." s then [s !! n | n <- [length "local." .. (length s) - 1]] else s
