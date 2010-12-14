module Calc where

import Types

type_mismatch f a b = Exception ("Type mismatch: " ++ 
                                                   (show a) ++ " " ++ f ++ " " ++ (show b))

-- calc: calls a function on the value of two Calculations, resulting in an exception if
--       either Calculation previously resulted in an exception
calc :: Calculation -> Calculation -> 
        (Value -> Value -> Calculation) -> Calculation
calc _ (Exception s) _ = Exception s
calc (Exception s) _ _ = Exception s
calc (Result a) (Result b) f = f a b
calc (Incomplete i) _ _ = Incomplete i
calc _ (Incomplete i) _ = Incomplete i

-- the following functions provide basic operations between Values, returning a Calculation
vadd, vsub, vprod, vdiv, vexp, veq, vgt, vlt :: Value -> Value -> Calculation
vadd (NumInt a) (NumInt b) = Result (NumInt (a + b))
vadd (Str a) (Str b) = Result (Str (a ++ b))
vadd (Str a) (NumInt b) = Result (Str (a ++ (show b)))
vadd (NumInt a) (Str b) = Result (Str ((show a) ++ b))
vadd (List a) (List b) = Result (List (a ++ b))
vadd a b = type_mismatch "+" a b
vsub (NumInt a) (NumInt b) = Result (NumInt (a - b))
vsub a b = type_mismatch "-" a b
vprod (NumInt a) (NumInt b) = Result (NumInt (a * b))
vprod a b = type_mismatch "*" a b
vdiv (NumInt a) (NumInt b) = Result (NumInt (a `div` b))
vdiv (NumFloat a) (NumFloat b) = Result (NumFloat (a / b))
vdiv a b = type_mismatch "/" a b
vexp (NumInt a) (NumInt b) = Result (NumInt (a ^ b))
vexp (NumFloat a) (NumFloat b) = Result (NumFloat (a ** b))
vexp a b = type_mismatch "^" a b
veq (NumInt a) (NumInt b) = Result (Bit (a == b))
veq (Str a) (Str b) = Result (Bit (a == b))
veq (Bit a) (Bit b) = Result (Bit (a == b))
veq a b = type_mismatch "=" a b
vgt (NumInt a) (NumInt b) = Result (Bit (a > b))
vgt a b = type_mismatch ">" a b
vlt (NumInt a) (NumInt b) = Result (Bit (a < b))
vlt a b = type_mismatch "<" a b
vand (Bit a) (Bit b) = Result (Bit (a && b))
vand a b = type_mismatch "&" a b
vor (Bit a) (Bit b) = Result (Bit (a || b))
vor a b = type_mismatch "|" a b
