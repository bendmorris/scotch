# std.decimal
# Decimal(a,b) represents a * 10 ^ -b

read_decimal(h:t, a) = case h of
                         "." -> Decimal(int(a + t), len(t)),
                         otherwise -> read_decimal(t, a + h)
read_decimal([], a) = Decimal(int(a), 0)
decimal(s) = read_decimal(s, "0")
                                       
print(Decimal(a,b)) = print(((if l > b then left(s, l - b) else "0") +
                               "." +
                               case str([for i in [2..b], "0"]) + str(right(s, b)) of 
                                 "" -> "0", 
                                 otherwise -> otherwise)
                              where s = str(a), l := len(s))

float(Decimal(a,b)) = (a * (10 ^ (-b)))

Decimal(a,b) + Decimal(c,d) = if b == d
                              then Decimal(a + c, b)
                              else if b < d
                              then Decimal(a * 10,b + 1) + Decimal(c, d)
                              else Decimal(a, b) + Decimal(c * 10, d + 1)

Decimal(a,b) - Decimal(c,d) = if b == d
                              then Decimal(a - c, b)
                              else if b < d
                              then Decimal(a * 10, b + 1) - Decimal(c, d)
                              else Decimal(a, b) - Decimal(c * 10, d + 1)
                              
Decimal(a,b) * Decimal(c,d) = Decimal(a * c, b + d)

Decimal(a,b) / Decimal(c,d) = decimal(str((float(a) / c) * (float(10 ^ d) / 10 ^ b)))

Decimal(a,b) == Decimal(c,d) = if b == d
                               then a == c
                               else if b < d
                               then Decimal(a * 10,b + 1) == Decimal(c, d)
                               else Decimal(a, b) == Decimal(c * 10, d + 1)
