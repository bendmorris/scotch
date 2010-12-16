# pi
pi = 3.141592654

# factorial
fact(n) = n * fact(n-1)
fact(1) = 1

# calculate the nth fibonacci number
fib'(a,b,n) = fib'(b,a+b,n-1)
fib'(a,b,0) = a
fib(n) = fib'(0,1,n)

# square root
sqrt(n) = n ^ 0.5

# arithmetic, sum, product, mean
add(x, y) = x + y
subtract(x, y) = x - y
multiply(x, y) = x * y
divide(x, y) = x / y
sum(h:t) = foldl(add, 0, h:t)
prod(h:t) = foldl(multiply, 0, h:t)
mean(h:t) = sum(h:t) / len(h:t)
mean([]) = 0
median'(h:t) = case len(h:t) of \
                 1 -> h, \
                 2 -> (h + head(t)) / 2.0, \
                 otherwise -> median(left(t, len(t) - 1))
median(h:t) = median'(sort(h:t))
median([]) = 0