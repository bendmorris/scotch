test(a, b) = if a == b then "yes" else a

tests := [if 1 + 1 == 2 then "yes" else "no"]
tests := tests + test("abc" + "def", "abcdef")
tests := tests + test(head("abcdef"), "a")
tests := tests + test(tail("abcdef"), "bcdef")

f(n) = n
g(n) = f(n) + 1
apply(f, x) = f(x)

tests := tests + test(apply(f, 1), 1)
tests := tests + test(apply(g, 1), 2)

tests := tests + test(split("a.b.c", "."), ["a","b","c"])

import std.math
tests := tests + test(filter(even, range(10)), [2,4,6,8,10])
tests := tests + test(foldl(add, 0, range(10)), 55)

apply(f, x, n) = f(x) + apply(f, x, n-1)
apply(f, x, 0) = 0
tests := tests + test(apply(f, 1, 3), 3)
tests := tests + test(apply(g, 1, 3), 6)

f(x, y) = x + y
a = f(1)
tests := tests + test(a(2), 3)

tests := tests + test(len("abcdefg"), 7)
tests := tests + test(left("abcdefg", 3), "abc")
tests := tests + test(int("1234567"), 1234567)
print tests
print (if all(for test in tests, if test == "yes" then true else false) 
        then "All tests passed."
        else "Some tests failed.")

print "Done!"
