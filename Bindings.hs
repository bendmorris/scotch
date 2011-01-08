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
import Types
import Hash

-- a list of identifiers (empty list for variables) and an expression containing them
type Call = ([Id], Expr)
-- binds an ID to a Call
type Binding = (Id, Call)
type VarDict = [[Binding]]

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
addBinding binding vars = [if n == hash
                           then binding : (removeBindingFrom binding (vars !! n))
                           else vars !! n
                           | n <- [0 .. (hashSize - 1)]]
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
