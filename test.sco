# Tests
test(a, b) = if a == b then "yes" else a

tests := [if 1 + 1 == 2 then "yes" else "no"]
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
tests += test(filter(even, range(10)), [2,4,6,8,10])
tests += test(foldl(add, 0, range(100)), 5050)

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

a = do b = 123;
       c = 456;
a

tests += test(b, 123)
tests += test(c, 456)

tests += test(median([5,3,2,4,1]), 3)
tests += test(mean([5,3,2,4,1]), 3)
tests += test(sort([5,4,3,2,1]), [1,2,3,4,5])

file_name = "test.sco"
file = <<file_name>>
r = read(file)
tests += test(split(r, "\n") @ 0, "# Tests")

print tests
print (if all([for test in tests, if test == "yes" then true else false])
        then "All tests passed."
        else "Some tests failed.")

print "Done!"
