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

module Calc where

import Types
import Hash

type_mismatch f a b = Exception $ "Type mismatch: " ++ 
                                  show a ++ " " ++ f ++ " " ++ show b
                                  
-- calc: calls a function on the value of two Calculations, resulting in an exception if
--       either Calculation previously resulted in an exception
calc :: Expr -> Expr -> (Value -> Value -> Expr) -> Expr
calc _ (Exception s) _ = Exception s
calc (Exception s) _ _ = Exception s
calc (Val a) (Val b) f = f a b

-- the following functions provide basic operations between Values, returning an Expr
-- addition
vadd, vsub, vprod, vdiv, vexp, veq, vgt, vlt :: Value -> Value -> Expr
vadd (NumInt a) (NumInt b) = Val (NumInt (a + b))
vadd (NumFloat a) (NumFloat b) = Val (NumFloat (a + b))
vadd (NumInt a) (NumFloat b) = Val (NumFloat ((realToFrac a) + b))
vadd (NumFloat a) (NumInt b) = Val (NumFloat (a + (realToFrac b)))
vadd (Str a) (Str b) = Val (Str (a ++ b))
vadd (Str a) (NumInt b) = Val (Str (a ++ (show b)))
vadd (NumInt a) (Str b) = Val (Str ((show a) ++ b))
vadd (Str a) (NumFloat b) = Val (Str (a ++ (show b)))
vadd (NumFloat a) (Str b) = Val (Str ((show a) ++ b))
vadd (List a) (List b) = Val (List (a ++ b))
vadd (Str a) (List []) = Val (Str a)
vadd (List []) (Str b) = Val (Str b)
vadd (List a) (v) = Val (List (a ++ [v]))
vadd (v) (List b) = Val (List ([v] ++ b))
vadd (Hash a) (Hash b) = Val (Hash [(b !! n) ++ (a !! n) | n <- [0..(hashSize - 1)]])
vadd a b = type_mismatch "+" a b
-- subtraction
vsub (NumInt a) (NumInt b) = Val (NumInt (a - b))
vsub (NumFloat a) (NumFloat b) = Val (NumFloat (a - b))
vsub (NumInt a) (NumFloat b) = Val (NumFloat ((realToFrac a) - b))
vsub (NumFloat a) (NumInt b) = Val (NumFloat (a - (realToFrac b)))
vsub a b = type_mismatch "-" a b
-- multiplication
vprod (NumInt a) (NumInt b) = Val (NumInt (a * b))
vprod (NumFloat a) (NumFloat b) = Val (NumFloat (a * b))
vprod (NumInt a) (NumFloat b) = Val (NumFloat ((realToFrac a) * b))
vprod (NumFloat a) (NumInt b) = Val (NumFloat (a * (realToFrac b)))
vprod (Str s) (NumInt b) = Val (Str (foldl (++) "" (take (fromIntegral b) (repeat s))))
vprod (NumInt b) (Str s) = Val (Str (foldl (++) "" (take (fromIntegral b) (repeat s))))
vprod (List l) (NumInt b) = Val (List (foldl (++) [] (take (fromIntegral b) (repeat l))))
vprod (NumInt b) (List l) = Val (List (foldl (++) [] (take (fromIntegral b) (repeat l))))
vprod a b = type_mismatch "*" a b
-- division
div_by_zero = Exception "Division by zero"
vdiv (NumInt a) (NumInt 0) = div_by_zero
vdiv (NumInt a) (NumFloat 0) = div_by_zero
vdiv (NumFloat a) (NumInt 0) = div_by_zero
vdiv (NumFloat a) (NumFloat 0) = div_by_zero
vdiv (NumInt a) (NumInt b) = Val (NumInt (a `div` b))
vdiv (NumFloat a) (NumFloat b) = Val (NumFloat (a / b))
vdiv (NumInt a) (NumFloat b) = Val (NumFloat ((realToFrac a) / b))
vdiv (NumFloat a) (NumInt b) = Val (NumFloat (a / (realToFrac b)))
vdiv a b = type_mismatch "/" a b
-- exponent
vexp (NumInt a) (NumInt b) = Val (NumInt (a ^ b))
vexp (NumFloat a) (NumFloat b) = Val (NumFloat (a ** b))
vexp (NumInt a) (NumFloat b) = Val (NumFloat ((realToFrac a) ** b))
vexp (NumFloat a) (NumInt b) = Val (NumFloat (a ** (realToFrac b)))
vexp a b = type_mismatch "^" a b
-- equality
veq (NumInt a) (NumInt b) = Val (Bit (a == b))
veq (NumFloat a) (NumFloat b) = Val (Bit (a == b))
veq (NumInt a) (NumFloat b) = Val (Bit ((realToFrac a) == b))
veq (NumFloat a) (NumInt b) = Val (Bit (a == (realToFrac b)))
veq (List a) (List b) = Val (Bit (a == b))
veq (Str a) (Str b) = Val (Bit (a == b))
veq (Bit a) (Bit b) = Val (Bit (a == b))
veq (Atom a b) (Atom c d) = Val (Bit (a == c && b == d))
veq (Hash a) (Hash b) = Val (Bit (a == b))
veq a b = Val (Bit False)
-- greater than
vgt (NumInt a) (NumInt b) = Val (Bit (a > b))
vgt (NumFloat a) (NumFloat b) = Val (Bit (a > b))
vgt (NumInt a) (NumFloat b) = Val (Bit ((realToFrac a) > b))
vgt (NumFloat a) (NumInt b) = Val (Bit (a > (realToFrac b)))
vgt a b = type_mismatch ">" a b
-- less than
vlt (NumInt a) (NumInt b) = Val (Bit (a < b))
vlt (NumFloat a) (NumFloat b) = Val (Bit (a < b))
vlt (NumInt a) (NumFloat b) = Val (Bit ((realToFrac a) < b))
vlt (NumFloat a) (NumInt b) = Val (Bit (a < (realToFrac b)))
vlt a b = type_mismatch "<" a b
-- binary and
vand (Bit a) (Bit b) = Val (Bit (a && b))
vand a b = type_mismatch "&" a b
-- binary or
vor (Bit a) (Bit b) = Val (Bit (a || b))
vor a b = type_mismatch "|" a b
