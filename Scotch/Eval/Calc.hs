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

module Scotch.Eval.Calc where

import Scotch.Types.Types
import Scotch.Types.Hash
import Scotch.Types.Exceptions
                                  
-- calc: calls a function on the value of two Calculations, resulting in an exception if
--       either Calculation previously resulted in an exception
calc :: Expr -> Expr -> (Bool -> Expr -> Expr -> Expr) -> Bool -> Expr
calc _ (Exception s) _ _ = Exception s
calc (Exception s) _ _ _ = Exception s
calc a b f strict = f strict a b

-- the following functions provide basic operations between Values, returning an Expr
-- addition
vadd, vsub, vprod, vdiv, vmod, vexp, veq, vgt, vlt :: Bool -> Expr -> Expr -> Expr
vadd _ (Val (NumInt a)) (Val (NumInt b)) = Val (NumInt (a + b))
vadd _ (Val (NumFloat a)) (Val (NumFloat b)) = Val (NumFloat (a + b))
vadd _ (Val (NumInt a)) (Val (NumFloat b)) = Val (NumFloat ((realToFrac a) + b))
vadd _ (Val (NumFloat a)) (Val (NumInt b)) = Val (NumFloat (a + (realToFrac b)))
vadd _ (Val (Str a)) (Val (Str b)) = Val (Str (a ++ b))
vadd False (Val (Str a)) (Val (NumInt b)) = Val (Str (a ++ (show b)))
vadd False (Val (NumInt a)) (Val (Str b)) = Val (Str ((show a) ++ b))
vadd False (Val (Str a)) (Val (NumFloat b)) = Val (Str (a ++ (show b)))
vadd False (Val (NumFloat a)) (Val (Str b)) = Val (Str ((show a) ++ b))
vadd _ (Val (List a)) (Val (List b)) = Val (List (a ++ b))
vadd _ (Val (Str a)) (Val (List [])) = Val (Str a)
vadd _ (Val (List [])) (Val (Str b)) = Val (Str b)
vadd _ (Val v) (Val (List b)) = Val (List (v : b))
vadd False (Val (List a)) (Val v) = Val (List (a ++ [v]))
vadd _ (Val (Hash a)) (Val (Hash b)) = Val $ Hash $ makeHash' [i | c <- b, i <- c] a
vadd _ a b = exTypeMismatch a b "+"
-- subtraction
vsub _ (Val (NumInt a)) (Val (NumInt b)) = Val (NumInt (a - b))
vsub _ (Val (NumFloat a)) (Val (NumFloat b)) = Val (NumFloat (a - b))
vsub _ (Val (NumInt a)) (Val (NumFloat b)) = Val (NumFloat ((realToFrac a) - b))
vsub _ (Val (NumFloat a)) (Val (NumInt b)) = Val (NumFloat (a - (realToFrac b)))
vsub _ a b = exTypeMismatch a b "-"
-- multiplication
vprod _ (Val (NumInt a)) (Val (NumInt b)) = Val (NumInt (a * b))
vprod _ (Val (NumFloat a)) (Val (NumFloat b)) = Val (NumFloat (a * b))
vprod _ (Val (NumInt a)) (Val (NumFloat b)) = Val (NumFloat ((realToFrac a) * b))
vprod _ (Val (NumFloat a)) (Val (NumInt b)) = Val (NumFloat (a * (realToFrac b)))
vprod _ (Val (Str s)) (Val (NumInt b)) = Val (Str (foldr (++) "" (take (fromIntegral b) (repeat s))))
vprod _ (Val (NumInt b)) (Val (Str s)) = Val (Str (foldr (++) "" (take (fromIntegral b) (repeat s))))
vprod _ (Val (List l)) (Val (NumInt b)) = Val (List (foldr (++) [] (take (fromIntegral b) (repeat l))))
vprod _ (Val (NumInt b)) (Val (List l)) = Val (List (foldr (++) [] (take (fromIntegral b) (repeat l))))
vprod _ (Val (Bit a)) (Val (NumInt (-1))) = Val (Bit (not a))
vprod _ (Val (NumInt (-1))) (Val (Bit a)) = Val (Bit (not a))
vprod _ a b = exTypeMismatch a b "*"
-- division
div_by_zero = Exception "Division by zero"
vdiv _ (Val (NumInt a)) (Val (NumInt 0)) = div_by_zero
vdiv _ (Val (NumInt a)) (Val (NumFloat 0)) = div_by_zero
vdiv _ (Val (NumFloat a)) (Val (NumInt 0)) = div_by_zero
vdiv _ (Val (NumFloat a)) (Val (NumFloat 0)) = div_by_zero
vdiv _ (Val (NumInt a)) (Val (NumInt b)) = Val (NumInt (a `div` b))
vdiv _ (Val (NumFloat a)) (Val (NumFloat b)) = Val (NumFloat (a / b))
vdiv _ (Val (NumInt a)) (Val (NumFloat b)) = Val (NumFloat ((realToFrac a) / b))
vdiv _ (Val (NumFloat a)) (Val (NumInt b)) = Val (NumFloat (a / (realToFrac b)))
vdiv _ a b = exTypeMismatch a b "/"
-- remainder
vmod _ (Val (NumInt a)) (Val (NumInt 0)) = div_by_zero
vmod _ (Val (NumInt a)) (Val (NumFloat 0)) = div_by_zero
vmod _ (Val (NumFloat a)) (Val (NumInt 0)) = div_by_zero
vmod _ (Val (NumFloat a)) (Val (NumFloat 0)) = div_by_zero
vmod _ (Val (NumInt a)) (Val (NumInt b)) = Val (NumInt (mod a b))
vmod _ (Val (NumInt a)) (Val (NumFloat b)) = Val (NumInt (mod a (truncate b)))
vmod _ (Val (NumFloat a)) (Val (NumInt b)) = Val (NumInt (mod (truncate a) b))
vmod _ (Val (NumFloat a)) (Val (NumFloat b)) = Val (NumInt (mod (truncate a) (truncate b)))
vmod _ a b = exTypeMismatch a b "%"
-- exponent
vexp _ (Val (NumInt a)) (Val (NumInt b)) = if b > 0 then Val (NumInt (a ^ b)) 
                                           else Val (NumFloat ((realToFrac a) ** (realToFrac b)))
vexp _ (Val (NumFloat a)) (Val (NumFloat b)) = Val (NumFloat (a ** b))
vexp _ (Val (NumInt a)) (Val (NumFloat b)) = Val (NumFloat ((realToFrac a) ** b))
vexp _ (Val (NumFloat a)) (Val (NumInt b)) = Val (NumFloat (a ** (realToFrac b)))
vexp _ a b = exTypeMismatch a b "^"
-- equality
veq _ (Val (NumInt a)) (Val (NumInt b)) = Val (Bit (a == b))
veq _ (Val (NumFloat a)) (Val (NumFloat b)) = Val (Bit (a == b))
veq _ (Val (NumInt a)) (Val (NumFloat b)) = Val (Bit ((realToFrac a) == b))
veq _ (Val (NumFloat a)) (Val (NumInt b)) = Val (Bit (a == (realToFrac b)))
veq _ (Val (List [])) (Val (Str "")) = Val (Bit (True))
veq _ (Val (Str "")) (Val (List [])) = Val (Bit (True))
veq _ a b = case a == b of
              True -> Val (Bit True)
              False -> Val (Bit False)
-- greater than
vgt _ (Val (NumInt a)) (Val (NumInt b)) = Val (Bit (a > b))
vgt _ (Val (NumFloat a)) (Val (NumFloat b)) = Val (Bit (a > b))
vgt _ (Val (NumInt a)) (Val (NumFloat b)) = Val (Bit ((realToFrac a) > b))
vgt _ (Val (NumFloat a)) (Val (NumInt b)) = Val (Bit (a > (realToFrac b)))
vgt _ (Val (Str a)) (Val (Str b)) = Val (Bit (a > b))
vgt _ a b = exTypeMismatch a b ">"
-- less than
vlt _ (Val (NumInt a)) (Val (NumInt b)) = Val (Bit (a < b))
vlt _ (Val (NumFloat a)) (Val (NumFloat b)) = Val (Bit (a < b))
vlt _ (Val (NumInt a)) (Val (NumFloat b)) = Val (Bit ((realToFrac a) < b))
vlt _ (Val (NumFloat a)) (Val (NumInt b)) = Val (Bit (a < (realToFrac b)))
vlt _ (Val (Str a)) (Val (Str b)) = Val (Bit (a < b))
vlt _ a b = exTypeMismatch a b "<"


-- validList: checks a list for exceptions
validList [] = Val (Bit True)
validList (h:t) = case h of                   
                   Exception e -> Exception e
                   Val (Undefined e) -> Exception e
                   otherwise -> validList t
                   
computableList [] = True
computableList (h:t) = case h of
                         Val (InvalidValue) -> False
                         Val v -> computableList t
                         otherwise -> False
