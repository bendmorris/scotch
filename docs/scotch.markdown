# Scotch Programming Language

This is the official documentation for the Scotch programming language.

[TOC]

## Datatypes


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
change.

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
