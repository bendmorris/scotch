tests := [if 1 + 1 == 2 then "yes" else "no"]
tests := tests + if "abc" + "def" == "abcdef" then "yes" else "no"
tests := tests + if head("abcdef") == "a" then "yes" else "no"
tests := tests + if tail("abcdef") == "bcdef" then "yes" else "no"

f(n) = n
g(n) = f(n) + 1
apply(f, x) = f(x)

tests := tests + if apply(f, 1) == 1 then "yes" else "no"
tests := tests + if apply(g, 1) == 2 then "yes" else "no"

import std.math
tests := tests + if filter(range(10), even) == [2,4,6,8,10] then "yes" else "no"
tests := tests + if foldl(add, 0, range(10)) == 55 then "yes" else "no"

apply(f, x, n) = f(x) + apply(f, x, n-1)
apply(f, x, 0) = 0
tests := tests + if apply(f, 1, 3) == 3 then "yes" else "no"
tests := tests + if apply(g, 1, 3) == 6 then "yes" else "no"

print tests
