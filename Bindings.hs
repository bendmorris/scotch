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
