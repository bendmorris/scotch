# these functions will be imported by the Scotch interpreter automatically

len(h:t) = 1 + len(t)
len([]) = 0

range(start, n, step) = start + 
                        if start + step > n then [] 
                                            else range(start + step, n, step)
range(start, n) = range(start, n, 1)
range(n) = range(n-1) + n
range(0) = []

head(h:t) = h
head([]) = []
tail(h:t) = t
tail([]) = []
reverse(h:t) = reverse(t) + h
reverse([]) = []

join(h:t, s) = h + if len(t) > 0 then s + join(t, s) else ""

all(h:t) = if h then all(t) else false
all([]) = true
any(h:t) = if h then true else any(t)
any([]) = false

prefix(a:b, c:d) = if a == c then prefix(b, d) else false
prefix([], c) = false
prefix(a, []) = true
suffix(a, c) = prefix(reverse(a), reverse(c))

contains(h:t, s) = if prefix(h:t, s) then true else contains(t, s)
contains([], s) = false

count(h:t, s) = (if prefix(h:t, s) then 1 else 0) + count(t, s)
count([], s) = 0

lstrip(h:t, s) = if contains(s, h) then lstrip(t, s) else h + t
lstrip([], s) = []
lstrip(h:t) = lstrip(h:t, " ")
rstrip(a, s) = reverse(lstrip(reverse(a), s))
rstrip(h:t) = rstrip(h:t, " ")
strip(h:t) = strip(h:t, " ")
strip(h:t, s) = lstrip(rstrip(h:t, s), s)


split(h:t, s) = (if h == s then "" + rest 
                           else [h + head(rest)] + tail(rest)) 
                           where rest := split(t, s)
split([], s) = []

replace(h:t, s, r) = if prefix(h:t, s) then r + replace(right(h:t, len(h:t) - len(s)), s, r) 
                                       else h + replace(t, s, r)
replace([], s, r) = []

only(h:t, s) = (if contains(s, h) then h else "") + only(t, s)
only([], s) = []

str(n) = "" + n
str(h:t) = str(h) + str(t)
str([]) = ""

int(h:t, n) = int(h) * (10 ^ n) + int(t, n-1)
int([], n) = 0
int(n) = int(head(split(str(n), ".")))
int(h:t) = (int(a, len(a) - 1)) where a = only(h:t, "0123456789")
int('0') = 0
int('1') = 1
int('2') = 2
int('3') = 3
int('4') = 4
int('5') = 5
int('6') = 6
int('7') = 7
int('8') = 8
int('9') = 9
int([]) = 0

float(n) = n * 1.0

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
