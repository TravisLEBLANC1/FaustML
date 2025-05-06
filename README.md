# FaustML

FaustML is a **first order simply typed ML**, with OCaml-compatible concrete syntax.
The project contains a typecheck and polytime check

Using a tiering algorithm, we guarantee that the program will execute in PTIME 
if we use total memoization.

## Build

to build the project use `dune build`
the only depedency is *OCamlgraph*

## Usage

`./faust file.ml` will check if the file.ml is corectly typed, and if it satisfy the condition to be executed in polytime

