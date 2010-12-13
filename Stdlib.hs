module Stdlib where

import Types

stdlib :: [Binding]
stdlib = [
          ("True",      ([], Val (Bit True))                    ),
          ("False",     ([], Val (Bit False))                   ),
          ("Null",      ([], Val Null)                          ),
          ("null",      ([], Val Null)                          ),
          ("pi",        ([], Val (Number 3.141592654))          ),
          --("fact",      ([Val (Number 1)], Val (Number 1))      ),
          ("fact",      (["n"], Prod (Var "n") (Func "fact" [Sub (Var "n") (Val (Number 1.0))])) )
          ]
