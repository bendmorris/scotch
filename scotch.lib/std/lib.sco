# these functions will be imported by the Scotch interpreter automatically

# Returns the length of a string or list.
len(h:t) = 1 + len(t)
len([]) = 0

# Returns a list of numbers from \start to \n, using step size \step.
range(start, n, step) = start + 
                        if start + step > n then [] 
                                            else range(start + step, n, step)
range(start, n) = range(start, n, 1)
range(n) = range(n-1) + n
range(0) = []

# Returns the first element in a string or list.
head(h:t) = h
head([]) = []
# Returns a list or string, minus the first element.
tail(h:t) = t
tail([]) = []
# Returns a list or string in reverse order.
reverse(h:t) = reverse(t) + h
reverse([]) = []

# Joins the members of a list, separating them by string \s.
join(h:t, s) = h + if len(t) > 0 then s + join(t, s) else ""

# Returns true only if all of the members in a list are true.
all(h:t) = if h then all(t) else false
all([]) = true
# Returns true if any of the members in a list are true.
any(h:t) = if h then true else any(t)
any([]) = false

# Checks whether string/list \c:d is a prefix of \a:b.
prefix(a:b, c:d) = if a == c then prefix(b, d) else false
prefix([], c) = false
prefix(a, []) = true
# Checks whether string/list \c is a prefix of \a.
suffix(a, c) = prefix(reverse(a), reverse(c))

# Returns true if \h:t contains element/sequence \s.
contains(h:t, s) = if prefix(h:t, s) then true else contains(t, s)
contains([], s) = false

# Returns the number of times \s appears in \h:t.
count(h:t, s) = (if prefix(h:t, s) then 1 else 0) + count(t, s)
count([], s) = 0

# Removes all characters in \s from the left of a string.
lstrip(h:t, s) = if contains(s, h) then lstrip(t, s) else h + t
lstrip([], s) = []
lstrip(h:t) = lstrip(h:t, " ")
# Removes all characters in \s from the right of a string.
rstrip(a, s) = reverse(lstrip(reverse(a), s))
rstrip(h:t) = rstrip(h:t, " ")
# Removes all characters in \s from the left and right of a string.
strip(h:t) = strip(h:t, " ")
strip(h:t, s) = rstrip(lstrip(h:t, s), s)

# Splits a string into a list of strings separated by character \s.
split(h:t, s) = (if h == s then "" + rest 
                           else [h + head(rest)] + tail(rest)) 
                           where rest := split(t, s)
split([], s) = []

# Replaces all instances of \s with \r.
replace(h:t, s, r) = if prefix(h:t, s) then r + replace(right(h:t, len(h:t) - len(s)), s, r) 
                                       else h + replace(t, s, r)
replace([], s, r) = []

only(h:t, s) = (if contains(s, h) then h else "") + only(t, s)
only([], s) = []

left(h:t, n) = h + left(t, n-1)
left(h:t, 0) = []
left([], n) = []
right(h:t, n) = reverse(left(reverse(h:t), n))

foldl(f, z, h:t) = foldl(f, f(z, h), t)
foldl(f, z, []) = z
foldr(f, z, h:t) = f(h, foldr(a, z, t))
foldr(f, z, []) = z

filter(f, h:t) = case f(h) of 
                   true -> h + filter(f, t),
                   otherwise -> filter(f, t)
filter(f, []) = []
