# Tests
test(a, b) = if a == b then "yes" else a

tests := [test(1 + 1, 2)]
tests += test("abc" + "def", "abcdef")
tests += test(head("abcdef"), "a")
tests += test(tail("abcdef"), "bcdef")

f(n) = n
g(n) = f(n) + 1
apply(f, x) = f(x)

tests += test(apply(f, 1), 1)
tests += test(apply(g, 1), 2)

tests += test(split("a.b.c", "."), ["a","b","c"])

import std.math
tests += test(filter(even, [1..10]), [2,4,6,8,10])
tests += test(foldl(x, y -> x + y, 0, [1..100]), 5050)

apply(f, x, n) = f(x) + apply(f, x, n-1)
apply(f, x, 0) = 0
tests += test(apply(f, 1, 3), 3)
tests += test(apply(g, 1, 3), 6)

f(x, y) = x + y
a = f(1)
tests += test(a(2), 3)

tests += test(len("abcdefg"), 7)
tests += test(left("abcdefg", 3), "abc")
tests += test(int("1234567"), 1234567)

a = do b = 123
       c = 456
a

tests += test(b, 123)
tests += test(c, 456)

tests += test(median([5,3,2,4,1]), 3)
tests += test(mean([5,3,2,4,1]), 3)
tests += test(sort([5,4,3,2,1]), [1,2,3,4,5])

file_name = "test.sco"
file = (<file_name>)
tests += test(split(read(file) + "abcdefg", "\n") @ 0, "# Tests")
r = read(file)
# This test doesn't yet pass, due to a known issue with iofile
# tests += test(split(r, "\n") @ 0, "# Tests")

f(n) = n
tests += test([for i in [1..2], f(i)], [1, 2])
tests += test([for i in [1..10], fib(i)], [1,1,2,3,5,8,13,21,34,55])

f(Apple(Banana "abc")) = "apple"
a = (Apple(Banana "abc"))
f(Apple(Banana(a,b,c))) = [a,b,c]
tests += test(f(a), "apple")
tests += test(f(Apple(Banana(1,2,3))), [1,2,3])

tests += test(apply(a -> a * 10, 10), 100)

b = 2
dict = {'a':1,'b':b, c=5}
tests += test([for i in ['a', 'b', 'c'], dict @ i], [1,2,5])

tests += test({} + {'a':1}, {'a':1})
tests += test(std.math.pi, pi)

apple(n) = Apple n
tests += test(apple([1,2,3]), Apple [1,2,3])
tests += test(apple("abc"), Apple "abc")

dict = {'add': a, b -> a + b, 'multiply': a, b -> a * b}
apply(f, a, b) = f(a, b)
tests += test(apply(dict @ 'add', 1, 2), 3)
tests += test(apply(dict @ 'multiply', 10, 2), 20)

import std.math as m
tests += test(m.pi, 3.141592654)

import std.decimal
tests += test(decimal("0.1") - decimal("0.05"), decimal("0.05"))

import std.fraction
tests += test(fraction("1/3") + fraction("1/2"), fraction("5/6"))

(Apple a) + (Banana b) <=> a + b
tests += test((Banana 10) + (Apple 12), 22)

a ** b = a ^ b
tests += test(2 ** 2, 4)

Apple a ** Apple b = a ^ b
tests += test((Apple 3) ** (Apple 2), 9)

print tests
print (if all([for test in tests, if test == "yes" then true else false])
        then "All " + len(tests) + " tests passed."
        else "Some tests failed.")

print "Done!"
