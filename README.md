
# Abstract Binding Trees

This is an implementation of abstract binding trees (ABT) in Ocaml.
A user of the library defines two modules, implementing 
the interfaces `SORT` and `OPERATOR`, and the library will from
this give:

* A module implementing multi-sorted abstract binding trees with
variables (subject to substitution) and symbols (subject to renameing).
Bound variables and and are considered equal up to renaming.
* A pretty printer and a parser.






