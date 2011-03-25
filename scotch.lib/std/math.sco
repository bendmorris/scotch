# pi and natural number
pi = 3.141592654
e = 2.71828183

# factorial
fact(n, a) = fact(n - 1, a * n)
fact(0, a) = a
fact(n) = fact(n, 1)

# calculate the nth fibonacci number
fib(a,b,n) = fib(b, a + b, n - 1)
fib(a,b,0) = a
fib(n) = fib(0, 1, n)

# square root
sqrt(n) = n ^ 0.5

# mean, median
mean(h + t) = sum(h + t) / len(h + t)
mean([]) = 0
median'(h + t) = case len(h + t) of 
                   1: h, 
                   2: (h + head(t)) / 2.0, 
                   otherwise: median'(left(t, len(t) - 1))
median'([]) = 0
median(h + t) = median'(sort(h + t))
median([]) = 0

# even, odd
even(n) = n mod 2 == 0
odd(n) = not even(n)
evens = filter(even, [2..])
odds = filter(odd, [1..])

# absolute value
abs(n) = if n < 0 then -n else n

# prime
prime(n) = if n < 2 then false
           else len(take 1 from divisors'(n)) == 0
primes = filter(prime, [2..])
divisors'(n) = [for i in (2 + [3 .. int(sqrt(n) + 1), 2]), i, n mod i == 0, n > 2]
divisors(n) = [for i in ([1 .. n - 1]), i, n mod i == 0, n > 0]
