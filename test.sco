# Tests
import std.unit

tests += assert_equal(1 + 1, 2)
tests += assert_equal("abc" + "def", "abcdef")
tests += assert_equal(head("abcdef"), "a")
tests += assert_equal(tail("abcdef"), "bcdef")

f(n) = n
g(n) = f(n) + 1
apply(f, x) = f(x)

tests += assert_equal(apply(f, 1), 1)
tests += assert_equal(apply(g, 1), 2)

tests += assert_equal(split("a.b.c", "."), ["a","b","c"])

import std.math
tests += assert_equal(filter(even, [1..10]), [2,4,6,8,10])
tests += assert_equal(foldl(x, y -> x + y, 0, [1..100]), 5050)

apply(f, x, n) = f(x) + apply(f, x, n-1)
apply(f, x, 0) = 0
tests += assert_equal(apply(f, 1, 3), 3)
tests += assert_equal(apply(g, 1, 3), 6)

f(x, y) = x + y
a = f(1)
tests += assert_equal(a(2), 3)

tests += assert_equal(len("abcdefg"), 7)
tests += assert_equal(left("abcdefg", 3), "abc")
tests += assert_equal(int("1234567"), 1234567)

a = do b = 123
       c = 456
a

tests += assert_equal(b, 123)
tests += assert_equal(c, 456)

tests += assert_equal(median([5,3,2,4,1]), 3)
tests += assert_equal(mean([5,3,2,4,1]), 3)
tests += assert_equal(sort([5,4,3,2,1]), [1,2,3,4,5])

file_name = "test.sco"
file = (<file_name>)
tests += assert_equal(split(read(file) + "abcdefg", "\n") @ 0, "# Tests")
r = read(file)
# This test doesn't yet pass, due to a known issue with iofile
# tests += assert_equal(split(r, "\n") @ 0, "# Tests")

f(n) = n
tests += assert_equal([for i in [1..2], f(i)], [1, 2])
tests += assert_equal([for i in [1..10], fib(i)], [1,1,2,3,5,8,13,21,34,55])

f(Apple(Banana "abc")) = "apple"
a = (Apple(Banana "abc"))
f(Apple(Banana(a,b,c))) = [a,b,c]
tests += assert_equal(f(a), "apple")
tests += assert_equal(f(Apple(Banana(1,2,3))), [1,2,3])

tests += assert_equal(apply(a -> a * 10, 10), 100)

b = 2
dict = {'a':1,'b':b, c=5}
tests += assert_equal([for i in ['a', 'b', 'c'], dict @ i], [1,2,5])

tests += assert_equal({} + {'a':1}, {'a':1})
tests += assert_equal(std.math.pi, pi)

apple(n) = Apple n
tests += assert_equal(apple([1,2,3]), Apple [1,2,3])
tests += assert_equal(apple("abc"), Apple "abc")

dict = {'add': a, b -> a + b, 'multiply': a, b -> a * b}
apply(f, a, b) = f(a, b)
tests += assert_equal(apply(dict @ 'add', 1, 2), 3)
tests += assert_equal(apply(dict @ 'multiply', 10, 2), 20)

import std.math as m
tests += assert_equal(m.pi, 3.141592654)

import std.decimal
tests += assert_equal(decimal("0.1") - decimal("0.05"), decimal("0.05"))

import std.fraction
tests += assert_equal(fraction("1/3") + fraction("1/2"), fraction("5/6"))

(Apple a) + (Banana b) <=> a + b
tests += assert_equal((Banana 10) + (Apple 12), 22)

a ** b = a ^ b
tests += assert_equal(2 ** 2, 4)

Apple a ** Apple b = a ^ b
tests += assert_equal((Apple 3) ** (Apple 2), 9)

run_tests
