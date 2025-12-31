# haskell

Use cabal to compile this, it's fairly automatic

## FuzzyMatch

Edit distance (aka Levenshtein distance) between two strings / many strings / best matching pairs of strings

The idea was to use it for joining Wikipedia tables together but it was just too slow

Uses lazy evaluation creatively, I would say

## Favs

Functions that have proven useful to me

## MHash

An interpreter for a language with Haskell-like syntax but dynamic

Uses type Dynamic for values

It has two parsing/printing systems

The one currently in use uses a single specification for both parsing and printing,
using an idea from invertible-syntax and partial-isomorphisms on hackage.com.
This system is in Parser7 and Iso

There is also an older one where the spec is separate, this is in MHash

## MHashDynamic3

An extended version of `Dynamic` from base, with multimethods and automatic conversion, and Eq, Ord and Show instances

Also contains a multidimensional array type `SubArrayD e`

`SubArrayD` can transpose dimensions in O(1) time. This makes it useful for complex maps and folds across dimensions

Each dimension can optionally have a `Dynamic` index (ie. making that dimension work like a Map)

Uses putGrid to display `SubArrayD`s with as many dimensions as you like

Lots of stuff in one file due to cyclic dependencies which Haskell doesn't do so well

## Matrix2

Matrix arithmetic and some solvers for simultaneous linear equations

## MyPretty2

A pretty printer. `putGrid` can adjust column widths of tables (ie. values of type `[[String]]`) automatically to get the best use of space

Got way to deep into this:

I think I found a general law:
No column A with less total text than B can end up wider than B
I can't think of a counterexample that would make sense

Is that true for rows as well?

Also, colWidths7 finds the ideal ratio for any two columns
It calculates the ratio of corresponding cells in each column
And then finds the ratio that has equal amounts of text in the limiting cells above and below that ratio
With three columns you want equal amounts of text in all the limiting cells of each column
If you put the columns together in order of total text, would that be ideal?

I think I found an analogy:
Gas makes no sense, text is more like incompressible liquid

Liquid in a compartmentalised tank where the walls can move left or right or up or down
according to whether theyre vertical or horizontal (without friction)
Somehow the vertical can move through the horizontal and vice versa

No gravity, no air pressure

the width has a fixed maximum ie. the thing is between four planes of infinite strength
that do not move

and the other two planes (horizontal) have some non-zero force pushing them together and
are able to move

And then... equalise the forces?


## Parser

An Earley parser

## Parser4

A parser/printer ie. same spec can both parse and print back out

## Parser6

Another one

## Parser7

An attempt at an Earley parser/printer extended with operations like Not, Monads

## Prolog

A very basic Prolog interpreter, can be parallelised

## Prolog1

A more complex Prolog interpreter that can be parallelised to a greater extent but nowhere near working

## Table/TableB

An in memory database. Uses a `Tree` for each record to join records in constant time. Can do fuzzy joins. Can do maps and folds (such as sum) over 
`Table`s. The folds follow the structure of the index, so that an index on two fields has subtotals as well as totals

## Tree

A container indexed by Int (ie. like an array) with O(log n) lookup and insert (ie. shifting ranges of indices to other places).
Due to the fact that it's a tree, multiple slightly different versions can share common values

Possible future extension: Use `Tree` as the behind the scenes store for `SubArrayD`, therefore giving it the same properties

## HTML

read/write HTML from the web/a filesystem

## HTMLB

HTML with ByteStrings

## Wiki/Wiki1

Uses the HTMLB module to scrape data tables from Wikipedia into tables held by TableB, join them up and write to disk

## Poker

Doesn't compile and so is not in the cabal file but when it did it played poker on Facebook