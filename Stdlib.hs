module Stdlib where

import Types

stdlib :: [Binding]
stdlib = [
          (Name "True",      ([], Val (Bit True))                    ),
          (Name "False",     ([], Val (Bit False))                   ),
          (Name "Null",      ([], Val Null)                          ),
          (Name "null",      ([], Val Null)                          ),
          (Name "pi",        ([], Val $ Number 3.141592654)          ),
          --("fact",      ([Val (Number 1)], Val (Number 1))      ),
          (Name "fact",      ([Name "n"], Prod (Var $ Name "n") 
                                               (Func (Name "fact") [Sub (Var $ Name "n") (Val $ Number 1.0)])) )
          ]
