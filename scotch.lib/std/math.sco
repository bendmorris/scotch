# pi and natural number
pi = 3.141592654
e = 2.71828183

# factorial
fact(n, a) = fact(n-1, a*n)
fact(0, a) = a
fact(n) = fact(n, 1)

# calculate the nth fibonacci number
fib'(a,b,n) = fib'(b,a+b,n-1)
fib'(a,b,0) = a
fib(n) = fib'(0,1,n)

# square root
sqrt(n) = n ^ 0.5

# mean, median
mean(h+t) = sum(h+t) / len(h+t)
mean([]) = 0
median'(h+t) = case len(h+t) of 
                 1: h, 
                 2: (h + head(t)) / 2.0, 
                 otherwise: median'(left(t, len(t) - 1))
median'([]) = 0
median(h+t) = median'(sort(h+t))
median([]) = 0

even(n) = int(n) / 2 == n / 2.0
odd(n) = not even(n)
