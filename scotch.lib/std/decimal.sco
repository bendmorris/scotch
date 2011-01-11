decimal_to_float(Decimal(a,b)) = (a * (10 ^ (-b)))

add(Decimal(a,b), Decimal(c,d)) = decimal_to_float(Decimal(a,b)) + 
                                  decimal_to_float(Decimal(c,d))
