
# Abstract Binding Trees

This is an implementation of abstract binding trees (ABT) in Ocaml.
A user of the library defines two modules defined by the interfaces
`SORT` and `OPERATOR`, and the library will from this give:

* A module implementing multi-sorted abstract binding trees with
variables (subject to substitution) and symbols (subject to renameing).
Terms are considered equal up to renaming of bound variables and symbols.
* A pretty printer and a parser.

## Credits
Based on ABT's as described in Robert Harper's Practical Foundations of Programming Languages and Jon Sterling's implementation in SML.




