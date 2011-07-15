# std.fraction
# Fraction(a, b) represents a / b as a fraction.

print(Fraction(a,b)) = print(a + "/" + b)

read_fraction(h:t, a) = case h of
                          "/" -> Fraction(int(a), int(t)),
                          otherwise -> read_fraction(t, a + h)
read_fraction([], a) = Fraction(int(a), 1)
fraction(s) = read_fraction(s, "0")

float(Fraction(a,b)) = float(a) / b

reduce_fraction(Fraction(a,b)) = 
  case len(take 1 from divisors) of
     0 -> Fraction(a,b),
     otherwise -> reduce_fraction(Fraction(a / divisors @ 0, b / divisors @ 0))
  where divisors := [for i in reverse([2..b]), i, a mod i == 0, b mod i == 0]
  
numerator(Fraction(a,b)) = a
denominator(Fraction(a,b)) = b

Fraction(a,b) + Fraction(c,d) = if b == d
                                then reduce_fraction(Fraction(a + c, b))
                                else Fraction(a * d, b * d) + Fraction(c * b, d * b)
Fraction(a,b) - Fraction(c,d) = if b == d
                                then reduce_fraction(Fraction(a - c, b))
                                else Fraction(a * d, b * d) - Fraction(c * b, d * b)
Fraction(a,b) * Fraction(c,d) = reduce_fraction(Fraction(a * c, b * d))
Fraction(a,b) / Fraction(c,d) = reduce_fraction(Fraction(a,b) * Fraction(d,c))
Fraction(a,b) == Fraction(c,d) = numerator(f1) == numerator(f2)
                                 and denominator(f1) == denominator(f2)
                                 where f1 := reduce_fraction(Fraction(a,b)),
                                       f2 := reduce_fraction(Fraction(c,d))
