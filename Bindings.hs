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

import Types

-- a list of identifiers (empty list for variables) and an expression containing them
type Call = ([Id], Expr)
-- binds an ID to a Call
type Binding = (Id, Call)
-- binding with associated scope (amount of whitespace)
type ScopedBinding = (Int, Binding)


-- given a scoped binding, returns an unscoped binding
unscope :: [ScopedBinding] -> [Binding]
unscope [] = []
unscope (h:t) = (snd h) : unscope t

rescope :: [Binding] -> [ScopedBinding]
rescope [] = []
rescope l = [(1, i) | i <- l]

-- removes all bindings that are no longer relevant at the current scope
scoped_bindings :: Int -> [ScopedBinding] -> [ScopedBinding]
scoped_bindings _ [] = []
scoped_bindings scope (h:t) = if scope < (fst h) then scoped_bindings scope t
                                                 else h : (scoped_bindings scope t)

-- adds a binding to a set of bindings, removing bindings that are now irrelevant
addBinding :: ScopedBinding -> [ScopedBinding] -> [ScopedBinding]
addBinding binding vars = binding : (removeBindingFrom binding vars)
samePattern [] [] = True
samePattern (h:t) (h':t') = case (h, h') of
                              (Name a, Name b) -> samePattern t t'
                              (Split a b, Split c d) -> samePattern t t'
                              (Pattern a, Pattern b) -> if a == b 
                                                        then samePattern t t'
                                                        else False
                              otherwise -> False
removeBindingFrom :: ScopedBinding -> [ScopedBinding] -> [ScopedBinding]
removeBindingFrom _ [] = []
removeBindingFrom binding (h:t) = if fst h == fst binding && 
                                     fst (snd h) == fst (snd binding) &&
                                     length (fst (snd (snd h))) ==
                                     length (fst (snd (snd binding))) &&
                                     samePattern (fst (snd (snd h))) (fst (snd (snd binding)))
                                  then removeBindingFrom binding t
                                  else h: (removeBindingFrom binding t)
addBindings (h:t) vars = addBinding h (addBindings t vars)
addBindings [] vars = vars
