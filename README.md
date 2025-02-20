# haskell

## MHash

An interpreter for a Haskell like language

It has two parsing/printing systems

The one currently in use uses a single specification for both parsing and printing,
using an idea from invertible-syntax and partial-isomorphisms on hackage.com.
This system is in Syntax3, SyntaxCIPU and SyntaxTH

There is also an older one where the spec is separate, this is in MHash

Uses type Dynamic for values

## MHashDynamic

An extended version of `Dynamic` from base, with multimethods and automatic conversion, and Eq, Ord and Show instances

Also contains a multidimensional array type `SubArrayD e` (there are too many interdependencies for it to be separate)

`SubArrayD` can transpose dimensions in O(1) time. This makes it useful for complex maps and folds across dimensions

Each dimension can optionally have a `Dynamic` index (ie. making that dimension work like a Map)

Uses putGrid to display `SubArrayD`s with as many dimensions as you like

## MyPretty2

A pretty printer. `putGrid` can adjust column widths of tables (ie. values of type `[[String]]`) automatically to get the best use of space

## Parser

An Earley parser

## Prolog

A very basic Prolog interpreter

## Table1G

An in memory database

## Tree

A container indexed by Int (ie. like an array) with O(log n) lookup and insert (ie. shifting ranges of indices to other places).
Due to the fact that it's a tree, multiple slightly different versions can share common values

Possible future extension: Use `Tree` as the behind the scenes store for `SubArrayD`, therefore giving it the same properties

## HTML

read/write HTML from the web/a filesystem

## Wiki/WikiG

Uses the `Https` module to scrape data tables from Wikipedia
