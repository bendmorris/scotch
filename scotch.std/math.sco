# pi
pi = 3.141592654

# factorial
fact(n) = n * fact(n-1)
fact(1) = 1

# calculate the nth fibonacci number
fib(n) = fibs(0,1,n)
fibs(a,b,n) = fibs(b,a+b,n-1)
fibs(a,b,0) = a

# square root
sqrt(n) = n ^ 0.5
