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

import std.math
tests := tests + test(filter(range(10), even), [2,4,6,8,10])
tests := tests + test(foldl(add, 0, range(10)), 55)

apply(f, x, n) = f(x) + apply(f, x, n-1)
apply(f, x, 0) = 0
tests := tests + test(apply(f, 1, 3), 3)
tests := tests + test(apply(g, 1, 3), 6)

print tests
