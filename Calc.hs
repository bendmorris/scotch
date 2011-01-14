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
calc :: Expr -> Expr -> (Expr -> Expr -> Expr) -> Expr
calc _ (Exception s) _ = Exception s
calc (Exception s) _ _ = Exception s
calc a b f = f a b

-- the following functions provide basic operations between Values, returning an Expr
-- addition
vadd, vsub, vprod, vdiv, vmod, vexp, veq, vgt, vlt :: Expr -> Expr -> Expr
vadd (Val (NumInt a)) (Val (NumInt b)) = Val (NumInt (a + b))
vadd (Val (NumFloat a)) (Val (NumFloat b)) = Val (NumFloat (a + b))
vadd (Val (NumInt a)) (Val (NumFloat b)) = Val (NumFloat ((realToFrac a) + b))
vadd (Val (NumFloat a)) (Val (NumInt b)) = Val (NumFloat (a + (realToFrac b)))
vadd (Val (Str a)) (Val (Str b)) = Val (Str (a ++ b))
vadd (Val (Str a)) (Val (NumInt b)) = Val (Str (a ++ (show b)))
vadd (Val (NumInt a)) (Val (Str b)) = Val (Str ((show a) ++ b))
vadd (Val (Str a)) (Val (NumFloat b)) = Val (Str (a ++ (show b)))
vadd (Val (NumFloat a)) (Val (Str b)) = Val (Str ((show a) ++ b))
vadd (Val (List a)) (Val (List b)) = Val (List (a ++ b))
vadd (Val (Str a)) (Val (List [])) = Val (Str a)
vadd (Val (List [])) (Val (Str b)) = Val (Str b)
vadd (Val (List a)) (Val v) = Val (List (a ++ [v]))
vadd (Val v) (Val (List b)) = Val (List ([v] ++ b))
vadd (Val (Hash a)) (Val (Hash b)) = Val (Hash [(b !! n) ++ (a !! n) | n <- [0..(hashSize - 1)]])
vadd a b = Func (Name "+") [a, b]
-- subtraction
vsub (Val (NumInt a)) (Val (NumInt b)) = Val (NumInt (a - b))
vsub (Val (NumFloat a)) (Val (NumFloat b)) = Val (NumFloat (a - b))
vsub (Val (NumInt a)) (Val (NumFloat b)) = Val (NumFloat ((realToFrac a) - b))
vsub (Val (NumFloat a)) (Val (NumInt b)) = Val (NumFloat (a - (realToFrac b)))
vsub a b = Func (Name "-") [a, b]
-- multiplication
vprod (Val (NumInt a)) (Val (NumInt b)) = Val (NumInt (a * b))
vprod (Val (NumFloat a)) (Val (NumFloat b)) = Val (NumFloat (a * b))
vprod (Val (NumInt a)) (Val (NumFloat b)) = Val (NumFloat ((realToFrac a) * b))
vprod (Val (NumFloat a)) (Val (NumInt b)) = Val (NumFloat (a * (realToFrac b)))
vprod (Val (Str s)) (Val (NumInt b)) = Val (Str (foldl (++) "" (take (fromIntegral b) (repeat s))))
vprod (Val (NumInt b)) (Val (Str s)) = Val (Str (foldl (++) "" (take (fromIntegral b) (repeat s))))
vprod (Val (List l)) (Val (NumInt b)) = Val (List (foldl (++) [] (take (fromIntegral b) (repeat l))))
vprod (Val (NumInt b)) (Val (List l)) = Val (List (foldl (++) [] (take (fromIntegral b) (repeat l))))
vprod a b = Func (Name "*") [a, b]
-- division
div_by_zero = Exception "Division by zero"
vdiv (Val (NumInt a)) (Val (NumInt 0)) = div_by_zero
vdiv (Val (NumInt a)) (Val (NumFloat 0)) = div_by_zero
vdiv (Val (NumFloat a)) (Val (NumInt 0)) = div_by_zero
vdiv (Val (NumFloat a)) (Val (NumFloat 0)) = div_by_zero
vdiv (Val (NumInt a)) (Val (NumInt b)) = Val (NumInt (a `div` b))
vdiv (Val (NumFloat a)) (Val (NumFloat b)) = Val (NumFloat (a / b))
vdiv (Val (NumInt a)) (Val (NumFloat b)) = Val (NumFloat ((realToFrac a) / b))
vdiv (Val (NumFloat a)) (Val (NumInt b)) = Val (NumFloat (a / (realToFrac b)))
vdiv a b = Func (Name "/") [a, b]
-- remainder
vmod (Val (NumInt a)) (Val (NumInt 0)) = div_by_zero
vmod (Val (NumInt a)) (Val (NumFloat 0)) = div_by_zero
vmod (Val (NumFloat a)) (Val (NumInt 0)) = div_by_zero
vmod (Val (NumFloat a)) (Val (NumFloat 0)) = div_by_zero
vmod (Val (NumInt a)) (Val (NumInt b)) = Val (NumInt (mod a b))
vmod a b = Func (Name "%") [a, b]
-- exponent
vexp (Val (NumInt a)) (Val (NumInt b)) = if b > 0 then Val (NumInt (a ^ b)) 
                                         else Val (NumFloat ((realToFrac a) ** (realToFrac b)))
vexp (Val (NumFloat a)) (Val (NumFloat b)) = Val (NumFloat (a ** b))
vexp (Val (NumInt a)) (Val (NumFloat b)) = Val (NumFloat ((realToFrac a) ** b))
vexp (Val (NumFloat a)) (Val (NumInt b)) = Val (NumFloat (a ** (realToFrac b)))
vexp (Val (Bit a)) (Val (Bit b)) = And (Val (Bit a)) (Val (Bit b))
vexp a b = Func (Name "^") [a, b]
-- equality
veq (Val (NumInt a)) (Val (NumInt b)) = Val (Bit (a == b))
veq (Val (NumFloat a)) (Val (NumFloat b)) = Val (Bit (a == b))
veq (Val (NumInt a)) (Val (NumFloat b)) = Val (Bit ((realToFrac a) == b))
veq (Val (NumFloat a)) (Val (NumInt b)) = Val (Bit (a == (realToFrac b)))
veq (Val (Str a)) (Val (Str b)) = Val (Bit (a == b))
veq (Val (List a)) (Val (List b)) = Val (Bit (a == b))
veq (Val (Hash a)) (Val (Hash b)) = Val (Bit (a == b))
veq (Val (HFunc a)) (Val (HFunc b)) = Val (Bit (a == b))
veq (Val (Bit a)) (Val (Bit b)) = Val (Bit (a == b))
veq a b = case a == b of
            True -> Val (Bit True)
            False -> Func (Name "==") [a, b]
-- greater than
vgt (Val (NumInt a)) (Val (NumInt b)) = Val (Bit (a > b))
vgt (Val (NumFloat a)) (Val (NumFloat b)) = Val (Bit (a > b))
vgt (Val (NumInt a)) (Val (NumFloat b)) = Val (Bit ((realToFrac a) > b))
vgt (Val (NumFloat a)) (Val (NumInt b)) = Val (Bit (a > (realToFrac b)))
vgt a b = Func (Name ">") [a, b]
-- less than
vlt (Val (NumInt a)) (Val (NumInt b)) = Val (Bit (a < b))
vlt (Val (NumFloat a)) (Val (NumFloat b)) = Val (Bit (a < b))
vlt (Val (NumInt a)) (Val (NumFloat b)) = Val (Bit ((realToFrac a) < b))
vlt (Val (NumFloat a)) (Val (NumInt b)) = Val (Bit (a < (realToFrac b)))
vlt a b = Func (Name "<") [a, b]


-- validList: checks a list for exceptions
validList [] = Val (Bit True)
validList (h:t) = case h of                   
                   Exception e -> Exception e
                   Val (Undefined e) -> Exception e
                   otherwise -> validList t
