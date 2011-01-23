<style type="text/css">
    h2, h3, h4, h5, h6 {margin: 30px 0 20px 0;}
    h1 {color: #081}
    h2 {color: #34b}
    h3, h4, h5, h6 {color: #3b4}
    pre, code {color: #444}
</style>

# Scotch Programming Language

This is the official documentation for the Scotch programming language.

[TOC]

## Data Types


### Numeric data

Two numeric types are supported by default in Scotch: integers and floats.
Type conversion between these two types is automatic.

The following operations are defined:

* Addition: `2 + 1`
* Subtraction: `2 - 1`
* Multiplication: `2 * 1`
* Division: `2 / 1`
* Exponent: `2 ^ 2`
* Remainder: `2 % 1` or `2 mod 1`
* Equality: `2 == 2`
* Greater than: `3 > 2`
* Less than: `2 < 3`
* Inequality: `1 != 2` or `1 not 2`

An operation between an integer and a float will be converted to a float.
Operations between only integers, for example, `3 / 2`, will produce an integer
result; in this case, `1` instead of the expected `1.5`. To get around this,
use `3 / 2.0` instead; the result will be expressed as a float.

Note that if highly accurate decimal calculations are needed, floats are not
sufficient as they are imperfect floating point representations of numbers.
You should use the decimal type defined in `std.decimal` or the fraction type 
defined in `std.fraction` instead; these types preserve accuracy.


### Boolean values

The reserved words `true` and `false` represent the boolean values.

Boolean operations include `and` (or `&`), `or` (or `|`), and `not`.


### Strings

Strings are denoted using `'single quotes'` or `"double quotes"`. The string
doesn't end at the end of the line, but at the closing quote, allowing for
multiline strings:

    "Here's
    an example
    of a multi-
    line string"
    
When a number is added to a string, the number is automatically converted to
its string representation; adding an empty list `[]` to a string makes no
change. Strings can also be added together: `"a" + "b" == "ab"`

Strings can be multiplied, producing multiple copies; for example, `"abc" * 3` 
will produce `"abcabcabc"`.


### Lists

Lists are a fundamental type in any functional language. In Scotch, lists are
immutable and can contain heterogeneous mixes of datatypes. Lists are 
designated with square brackets, like so:

    [1, 2, 3, "a", "b", "c"]
    
List members are accessed with the `@` operator: `[1, 2] @ 0` returns the first
element, `1`. Using the `@` operator followed by a list will access several 
members at once: `[1, 2, 3] @ [0, 1]` returns the first two elements of the 
list, or `[1, 2]`.

Lists can be multiplied, producing multiple copies; for example, `[1,2] * 3` 
will produce `[1,2,1,2,1,2]`.

Adding a single value to a list will add that value to the list. For example,

    [1,2,3] + 4 == [1,2,3,4]

To add multiple values to a list, add two lists together. For example,

    [1,2,3] + [4,5,6] == [1,2,3,4,5,6]
    
Importantly, there are no "tuples" in Scotch. A tuple is generally defined as 
an immutable list of heterogeneous datatypes; since Scotch lists fit this
definition, lists can be used in place of tuples.

Ranges can be generated automatically, using `..`: `[1..10]` returns the 
numbers from 1 to 10. Step size can also be defined: `[2..10,2]` returns every
2nd number. If no ending number is designated (`[1..]`), an infinite list is
returned; this must be used in combination with [take](#take) or it will
never end.


### Hash tables

Hash tables are unordered collections of key-value pairs, allowing values to be
looked up quickly by key. Hash tables use the following syntax:

    hash = {1:1, 2:2, 'a':3, 'b':4, name="Bob", age=23}
    
These values can be looked up using the `@` operator, i.e. `hash @ 'name'` or
`hash @ 'b'`.

Any datatype can be used as a hash key; it is important to note that the type 
will be converted to a string, so `hash @ 1` and `hash @ '1'` will refer to the 
same value in the hash table.

To add to a hash table, add two hash tables together. For example,

    {a=1, b=2} + {c=3} == {a=1, b=2, c=3}


### Files

File objects are created with angled brackets around a filename (as a string)
like so: `<"test.txt">`.

The following are examples of file operations:

    file = <"test.txt">
    read(file)
    write(file, "blah blah")
    append(file, "blah blah")
    
`read(file)` returns the contents of a file, as a string.


### Procs

A proc is an imperative-style procedure, which evaluates expressions in
sequence. They are defined with the `do` keyword like this:

    do print 1
       print 2
       print 3
       
It is important that the left sides of the expressions be lined up; this
designates what expressions should be part of the proc.

The `std.lib.execute` function evaluates each expression in a list. Passing a
list of procs will evaluate each proc in sequence.


### Threads

Using the `thread` keyword followed by a proc will execute the proc 
cocurrently, creating a lightweight thread.


### Data constructors

Custom data constructors can be used without being defined. A data constructor
is an atom (an identifier in CamelCase) followed by either a single value or
a comma-separated list of values, surrounded by parentheses.

Here are some examples:

    Dog "Max"
    Cat "Sandy"
    Decimal(10, 5)
    Person("Sam", 15, [1,2,3])


## Modules

A Scotch code file is called a module. Modules can be imported using the 
`import` keyword, like so:

    import std.math
    import test
    
Periods in a module name designates directory structure, so `std.math` would be 
found in `std/math.sco`. The module will be searched for first in the current
directory, then in the `scotch.lib` directory.
    
When a module is imported, it is completely executed; any native 
variable/function definitions from that module are then added to the current 
definitions.


## Variables

Scotch defines variables using both lazy and eager strategies.


### Lazy definition

    a = b + 1
    
When the variable `a` is found, it will be replaced by the current value of
`b + 1`. As `b` changes, `a` will also change.


### Eager definition

    a := b + 1
    
This defines `a` to be the value of `b + 1` at the time it was defined. So, if
`b` changes after this definition, `a` will remain the same.


### Temporary definition
    
    a + b
    where a = 1, b = 2

`where` creates a temporary variable definition, valid only for the preceding 
expression.


## Functions


### Function definition

    f(n) = n * 10
    f(a, b) = a + b
    

#### Pattern matching

    f(0) = "The argument is zero!"
    
Only when function `f` is called with one argument, `0`, will it return this
definition.

    len(head+tail) = 1 + len(tail)
    len([]) = 0
    
This is a recursive list (or string) length function. The first definition will
match either a list or a string; `head` will be defined as the first element or
character, and `tail` will be defined as everything else. The function will be
called again on everything but the first element, over and over, until it is
called on an empty list (or string).

    f(Dog d) = "A dog named " + d
    
This function definition will apply only when f is called with the value 
`Dog (something)` (a [data constructor](#data-constructors)). The value 
following the atom `Dog` will be set as `d`.


### Higher order functions/partial application

Functions can be passed as arguments to other functions. For example, with a
function `prime` that returns true if a number is prime,

    filter(prime, [1..100])
    
will return all prime numbers from 1 to 100.

"Partially applied functions" are functions that will be called with additional
arguments in the future. For example:

    add(x, y) = x + y
    apply(f, x) = f(x)
    apply(add(10), 5)
    
`add(10)` would normally not be a valid function call because there is no
definition of `add` that takes only one argument. But when `add(10)` is called
with another argument, 5, the result is 10 + 5, or 15.


### Operator definition

Custom binary operators can be defined as if they were functions. For example:

    a ** b = a ^ b
    
We've just defined the (previously undefined) ** operator as another exponent 
operator.

Addition between Apples and Bananas is undefined and will result in an 
exception. We can define this operation like so:

    Apple a + Banana b = Strawberry (a + b)
    
Now, the expression `(Apple 1) + (Banana 2)` will evaluate to `Strawberry 3`.

Commutative operators can be defined using `<=>` so that redundant function
definitions aren't necessary.

    Apple a + Banana b <=> Strawberry (a + b)
    
This example will match both `(Apple 1) + (Banana 2)` and 
`(Banana 2) + (Apple 1)`.


### Anonymous functions

Anonymous functions are expressed like this:

    x, y -> x + y
    
This sample represents an anonymous addition function. It takes two arguments,
`x` and `y`, and returns `x + y`.

These functions can be used in place of regularly defined functions. For
example, `reduce(x, y -> x + y, [1..10], 0)` will sum all numbers from 1 to 10.


## Other Expressions


### Skip

`skip` does nothing.


### If

    if true then "yes" else "no"


### Case

Case expressions employ [pattern matching](#pattern-matching) similar to
functions.

    case v of
      Apple a: a,
      Banana b: b,
      otherwise: otherwise
      
In this example, if `v` was `Apple 1`, it would match the first case; if it
is neither an Apple nor a Banana, it will match to `otherwise` (which is really
just a variable identifier, so the value becomes bound to the variable called
`otherwise` when the expression is evaluated.)


### Take

`take` is used to take the first `n` elements from a list. For example,

    take 10 from [1..]
    
will return the first 10 elements in the infinite list `[1..]`.


### Value conversion

Values can be explicitly converted to an integer, float, or string. For 
example:

    str(1)
    float(1)
    int(1.2)
    int('1')
    

### Input/output

    print "Hello world!"
    print "What's your name?"
    name := input
    print "Hello, " + name + "."
    
The `print` statement actually calls the function `show` on whatever it's
displaying. This allows you to define custom instances of `show`:

    show(Dog d) = "A dog named " + d
    print Dog "Max"
    
    
### List comprehension

List comprehensions allow quick, easy construction of lists by iterating over 
a collection.

    [for i in [1..10], j in [1..20], i * j, i > 5, j < 9]
    
This list comprehension returns `i * j` for every `i` from 1 to 10 and every
`j` from 1 to 20, but only if `i` is more than 5 and `j` is less than 9.

The following types can be used as collections:

* [Lists](#lists)
* [Strings](#strings) (each character)
* [Hash tables](#hash-tables) (treated as an unordered list of [key, value])
* [Files](#files) (iterates over the contents of the file as a string, line by line)


## Standard Library


### std.lib

The `std.lib` module contains basic, common functions. This module is 
automatically imported, so its definitions are always available.


### std.math

Contains various useful mathematical functions.


### std.decimal

Defines the `Decimal` data constructor, for high precision decimal arithmetic.
Decimals can be created using the `decimal` function, i.e. `decimal("0.501")`.
Decimal arithmetic:

    decimal("0.5") + decimal("0.1") == decimal("0.6")


### std.fraction

Defines the `Fraction` data constructor, for fractional arithmetic. Fractions
can be created using the `fraction` function, i.e. `fraction("11/32")`.
Fraction arithmetic:

    fraction("1/6") + fraction("1/7") == fraction("13/42")


### std.units

SI unit conversion. Data constructors representing unit magnitude, i.e.
`Kilo 1` or `Deci 2`, can be converted to and from other SI units like so:

    convert_unit(Mega 2, to_kilo) == Kilo 2000.0


### std.unit

A very simple unit testing framework.
