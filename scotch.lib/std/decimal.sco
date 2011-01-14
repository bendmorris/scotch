read_decimal(h+t, a) = case h of
                         ".": Decimal(int(a + t), len(t)),
                         otherwise: read_decimal(t, a + h)
read_decimal([], a) = Decimal(0, 0)
decimal(s) = read_decimal(s, "0")
                                       
show(Decimal(a,b)) = (if a < b
                      then show(Decimal(a * 10, b - 1))
                      else left(s, l - b) + "." + right(s, b))
                     where s = str(a), l := len(s)

decimal_to_float(Decimal(a,b)) = (a * (10 ^ (-b)))
Decimal(a,b) + Decimal(c,d) = if b == d
                              then Decimal(a + c, b)
                              else if b < d
                              then Decimal(a*10,b+1) + Decimal(c,d)
                              else Decimal(a,b) + Decimal(c*10,d+1)

Decimal(a,b) - Decimal(c,d) = if b == d
                              then Decimal(a - c, b)
                              else if b < d
                              then Decimal(a*10,b+1) - Decimal(c,d)
                              else Decimal(a,b) - Decimal(c*10,d+1)

Decimal(a,b) == Decimal(c,d) = show(Decimal(a,b)) == show(Decimal(c,d))
