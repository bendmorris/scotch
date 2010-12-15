# these functions will be imported automatically

len(h:t) = 1 + len(t)
len([]) = 0

head(h:t) = h
tail(h:t) = t
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

find(h:t, s) = if prefix(h:t, s) then true else find(t, s)
find([], s) = false

count(h:t, s) = (if prefix(h:t, s) then 1 else 0) + count(t, s)
count([], s) = 0

lstrip(h:t) = lstrip(h:t, " ")
lstrip(h:t, s) = if find(s, h) then lstrip(t, s) else h + t
rstrip(h:t) = rstrip(h:t, " ")
rstrip(a, s) = reverse(lstrip(reverse(a), s))
strip(h:t) = strip(h:t, " ")
strip(h:t, s) = lstrip(rstrip(h:t, s), s)

only(h:t, s) = (if find(s, h) then h else "") + only(t, s)
only([], s) = []

sum(h:t) = h + sum(t)
sum([]) = 0
prod(h:t) = h * prod(t)
prod([]) = 1
mean(h:t) = sum(h:t) / len(h:t)
mean([]) = 0

str(n) = "" + n
str(h:t) = str(h) + str(t)
str([]) = ""

int(h:t, n) = int(h) * (10 ^ n) + int(t, n-1)
int([], n) = 0
int(h:t) = int(only(h:t, "0123456789"), len(only(h:t, "0123456789")) - 1)
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

left(h:t, n) = h + left(t, n-1)
left(h:t, 0) = []
left([], n) = []
right(h:t, n) = reverse(left(reverse(h:t), n))
