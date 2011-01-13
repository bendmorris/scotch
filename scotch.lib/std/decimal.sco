read_decimal(h+t, a) = case h of
                         ".": Decimal(int(a + t), len(t)),
                         otherwise: read_decimal(t, a + h)
read_decimal([], a) = Decimal(0, 0)
decimal(s) = read_decimal(s, "0")
                                       
show(Decimal(a,b)) = (if a < b
                      then show(Decimal(a * 10, b - 1))
                      else left(s, l - b) + "." + right(s, b))
                     where l = len(s), s = str(a)

decimal_to_float(Decimal(a,b)) = (a * (10 ^ (-b)))
add(Decimal(a,b), Decimal(c,d)) = if b == d
                                  then Decimal(a + c, b)
                                  else if b < d
                                  then add(Decimal(a*10,b+1), Decimal(c,d))
                                  else add(Decimal(a,b), Decimal(c*10,d+1))

subtract(Decimal(a,b), Decimal(c,d)) = if b == d
                                       then Decimal(a - c, b)
                                       else if b < d
                                       then subtract(Decimal(a*10,b+1), Decimal(c,d))
                                       else subtract(Decimal(a,b), Decimal(c*10,d+1))
eq(Decimal(a,b), Decimal(c,d)) = decimal_to_float(Decimal(a,b)) == decimal_to_float(Decimal(c,d))
