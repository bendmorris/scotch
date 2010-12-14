module Stdlib where

import Types

stdlib :: [Binding]
stdlib = [
          (Name "pi",        ([], Val $ Number 3.141592654)          ),
          (Name "fact",      ([Pattern (Number 1)], Val (Number 1))  ),
          (Name "fact",      ([Name "n"], Prod (Var $ Name "n") 
                                               (Func (Name "fact") [Sub (Var $ Name "n") (Val $ Number 1.0)])) )
          ]
