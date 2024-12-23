## DB-Relations

DB-Relations is a relational database theory library written in Haskell. It focuses on implementing
datatypes and algorithms important in the theory of database normalization - that is, reducing
redundant information within a database. The library is split into four modules:

- Data.Relations

This module contains the datatypes used throughout the library and some elementary functions tied to their uses.

- Data.Relations.Dependencies

This module contains basic tools for manipulating functional dependencies, such as calculating attribute closures 
and other significant attributes of covers. 

- Data.Relations.Normalization

This module contains functions which check normalization forms of relations up to and including Boyce-Codd Normal Form.
It does not contain functions to decompose relations into these forms, as those are contained in the next module.

- Data.Relations.Decomposition

This module contains functions focused around decomposing relations into sets of smaller relations to reduce redundancy.
It implements decompositions for 2NF, 3NF, and BCNF, in addition to checks of common properties of decompositions.

In addition, the repository contains a command-line app to make use of the library easier, as well as a simple testing file
and sample input file. These are located in Main.hs and under Data.Testing respectively.

# Getting Started

The library is dependent on the packages `mtl` and `containers`. 
In addition, the app requires `parsec` and `text` are installed.

The project is configured through cabal, so if `cabal` is installed the application
can be run simply by opening a terminal in the base directory of the app and running
```cabal run``` (optionally, use `cabal run -v0` for silent compilation). 

Alternatively, the app or library functions can be run through ghci. Simply load 
Data/Relations.hs, Data/Relations/*.hs, and Main.hs, and then import the desired modules.
One helpful pattern is `rel = snd $ extract relation "R(<attributes>) <fds>"`, which allows 
for quickly defining relations within ghci for testing.

# Application grammars

In general, the application follows a very simple grammar for importing. Relations are listed by
`<relation> ::= <name>(<attributes>) <fds>`
where name is a single identifier, `<attributes>` is a comma-separated list of identifiers, 
and `<fds>` is a whitespace-separated list of `<fd>` instances, where `<fd>` is
`<fd> ::= <attributes> -> <attributes>`
For example, "R1(A,B,C,D) A,B->C D->A". The application uses the same general grammar. When a list
is requested it should be provided in space separated format. This is mainly relevant when adding or 
removing FDs, or selecting a decomposition.