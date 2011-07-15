rule infinite => infinity() + x = infinity,
                 x + infinity() = infinity + x,
                 infinity() * x = if x is 0 
                                  then 0
                                  else if x > 0 then infinity else neg_infinity,
                 x * infinity() = infinity * x,
                 infinity() / x = infinity,
                 x / infinity() = 0,
                 infinity() - x = infinity,
                 x - infinity() = neg_infinity
