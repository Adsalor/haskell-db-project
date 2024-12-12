## DB-Relations
======

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
and sample input file. These are located under Data.Relations.App and Data.Testing respectively.

# Getting Started

The library is dependent on the cabal packages `mtl` and `containers`. 
In addition, the app requires `parsec` and `text` be installed.

Make sure that your project repository contains a README.md. It should briefly 
describe the main components and the order in which we should look at them as well as list 
additional libraries and special instructions for compilation, building and running the project. 
Add config files listing dependencies, so that it is easy to build and run the project on our end.  

test