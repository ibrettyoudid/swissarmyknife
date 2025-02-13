# haskell

MHash
An interpreter for a Haskell like language
It has two parsing/printing systems
The one currently in use uses a single specification for both parsing and printing,
using an idea from invertible-syntax and partial-isomorphisms on hackage.com
This system is in Syntax3, SyntaxCIPU and SyntaxTH
There is also an older one where the spec is separate, this is in MHash
Uses type Dynamic for values

MHashDynamic
An extended version of Dynamic from base, with multimethods and automatic conversion, and Eq, Ord and Show instances
Also contains a multidimensional array type (there are too many interdependencies for it to be separate)
This array type (SubArrayD) can transpose dimensions in O(1) time
This makes it useful for complex maps and folds across dimensions
Each dimension can optionally have a Dynamic index (ie. making that dimension work like a Map)
Uses putGrid to display SubArrayDs with as many dimensions as you like

MyPretty2
A pretty printer. Does very nice grids with putGrid

Parser
An Earley parser

Prolog
A very basic Prolog interpreter

Tree
A container indexed by Int with O(log n) lookup and update

