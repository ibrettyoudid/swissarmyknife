# swissarmyknife

This is written in Haskell.
To get the Haskell tool chain, I used ghcup at https://www.haskell.org/ghcup/.
Linux is required for some parts. I use EndeavourOS.

Download cabal, ghc version 9.6.7 and whatever else you want.
Use cabal to compile this, it's fairly automatic. It will first download assorted libraries that are required for my code to work.

```
git clone https://github.com/ibrettyoudid/swissarmyknife
cabal build
```

It's being updated regularly so it's possible some bugs may slip in with the features

## FuzzyMatch

Edit distance (aka Levenshtein distance) between two strings / many strings / best matching pairs of strings.

The idea was to use it for joining Wikipedia tables together, SQL style.
I thought I could get good performance by using A* search over the whole problem, such that the closest matches would be found first, due to the way A* works.

Uses lazy evaluation creatively, I would say.

## Wiki/Wiki1

Uses the HTMLB module to scrape data tables from Wikipedia into tables held by TableB, join them up and writes to disk.
Caches them in a directory `.cache` so that they may be edited if errors occur.
Seems to work fine now without editing though.
Works on the Lists of countries and dependencies page, but presumably can be adapted to work on any of the Lists of lists pages.
Currently saves each page as a CSV file, but can also do HTML, and the code is there to make one big table.

To run:

```
cabal repl
:mod + Wiki HTMLB
m <- nm
wc m
```

## Favs

Functions that have proven useful to me.

## MHash

An interpreter for a language with Haskell-like syntax but dynamic.

Uses type Dynamic for values.

It has two parsing/printing systems.

The one currently in use uses a single specification for both parsing and printing,
using an idea from invertible-syntax and partial-isomorphisms on hackage.com.
This system is in Parser7 and Iso.

There is also an older one where the spec is separate, this is in MHash.

## MHashDynamic3

An extended version of `Dynamic` from base, with multimethods and automatic conversion, and Eq, Ord and Show instances.

Also contains a multidimensional array type `SubArrayD e`

`SubArrayD` can transpose dimensions in O(1) time. This makes it useful for complex maps and folds across dimensions.

Each dimension can optionally have a `Dynamic` index (ie. making that dimension work like a Map).

Uses putGrid to display `SubArrayD`s with as many dimensions as you like.

Lots of stuff in one file due to cyclic dependencies which Haskell doesn't do so well.

## Matrix2

Matrix arithmetic and some solvers for simultaneous linear equations.

## MyPretty2

A pretty printer. `putGrid` can adjust column widths of tables (ie. values of type `[[String]]`) automatically to get the best use of space.

Got way to deep into this, due to the desire to answer the question, can there be multiple minima?

Trying to find something to impress people like you with I think.

I think I found a general law:
No column A with less total text than B can end up wider than B.
I can't think of a counterexample that would make sense.

Is that true for rows as well?

Also, colWidths7 finds the ideal ratio for any two columns.
It calculates the ratio of corresponding cells in each column.
And then finds the ratio that has equal amounts of text in the limiting cells above and below that ratio.
With three columns you want equal amounts of text in all the limiting cells of each column.
If you put the columns together in order of total text, would that be ideal?

I think I found an analogy:
Gas makes no sense, text is more like incompressible liquid.

Liquid in a compartmentalised tank where the walls can move left or right or up or down.
according to whether theyre vertical or horizontal (without friction).
Somehow the vertical can move through the horizontal and vice versa.

No gravity, no air pressure.

the width has a fixed maximum ie. the thing is between four planes of infinite strength
that do not move.

and the other two planes (horizontal) have some non-zero force pushing them together and
are able to move.

And then... equalise the forces?


## Parser

An Earley parser
I would say these parsers are pretty powerful, although they need work.

## Parser3

An evolution of Parser, with operations like Many and Monads

## Parser4

A parser/printer ie. same spec can both parse and print back out
It uses the Dynamic type from MHashDynamic

## ID3

Uses attoparsec to read the metadata in MP3 files and standard IO to write them out

## ID3P4

A parser using the above module to read and write mp3 files

## Parser6

Another parser/printer. This uses GADTs to be type safe

## ID3P6

A parser using the above module to read and write mp3 files

## Parser7

A different attempt at an Earley parser/printer extended with operations like Many, Not, Monads

## Prolog

A very basic Prolog interpreter, can be parallelised I think

## Prolog1

A more complex Prolog interpreter that can be parallelised to a greater extent but nowhere near working

## Prolog3

A very basic Prolog interpreter using the logict package

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

## Chess

The start of a chess engine

## Dataflow2

The start of a programming language which the user can edit graphically

## Atlas

Written to overlay bitmap maps on each other, changing the projection and other parameters

## Anagram

Anagrams
This actually works

## Crossword

Crosswords.
This actually works.
Solves crosswords.
Can also create Sator squares and variations as in the film Tenet.
A Sator square is an interlocking square of words.

```
SATOR
AREPO A city in Ukraine
TENET A principle or belief
OPERA
ROTAS
```

This is the famous example from the film, I believe some of the words are Latin, though.

```
STANG A forked staff
TENON A type of saw
ANANA A pineapple!
NONET A group of nine
GNATS Insects
```

This is English (I swear) and was found with this code. I never managed to find a larger one that's the same backwards and forwards like the original.

But this is a 9x9 one with less symmetry I found with this code:

```
NECESSISM extreme determinism
EXISTENCE shared reality
CIRCUMFER to carry around
ESCARPING to form a steep slope
STURNIDAE starling family
SEMPITERN eternal
INFIDELIC of an infidel
SCENARISE to create a scenario
MERGENCES the act of merging, plural
```

I've improved the code a bit, since. I might try for a 10x10 one

## Poker

Poker bot for Facebook
Uses a replacement Javascript file `bundle.min.js` that you set up in your browser, to make the browser talk to the Haskell code via a socket

